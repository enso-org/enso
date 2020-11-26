package org.enso.projectmanager.infrastructure.languageserver
import java.io.PrintWriter
import java.lang.ProcessBuilder.Redirect
import java.util.UUID
import java.util.concurrent.Executor

import akka.actor.{Actor, ActorRef, Cancellable, Props, Stash}
import akka.pattern.pipe
import org.enso.loggingservice.LogLevel
import org.enso.projectmanager.data.Socket
import org.enso.projectmanager.infrastructure.http.AkkaBasedWebSocketConnectionFactory
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProcess.{
  Kill,
  ServerTerminated,
  ServerThreadFailed,
  Stop
}
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerSupervisor.{
  HeartbeatReceived,
  ServerUnresponsive
}
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagerMixin
import org.enso.projectmanager.versionmanagement.DistributionConfiguration
import org.enso.runtimeversionmanager.runner.{LanguageServerOptions, Runner}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{
  ExecutionContext,
  ExecutionContextExecutor,
  Future,
  Promise
}
import scala.util.Using

/** An Actor that manages a single Language Server process.
  *
  * It starts the process upon creation and notifies the parent once the process
  * has finished booting.
  *
  * @param progressTracker an [[ActorRef]] that will get progress updates
  *                        related to initializing the engine
  * @param descriptor a LS descriptor
  * @param rpcPort port to bind for RPC connections
  * @param dataPort port to bind for binary connections
  * @param bootTimeout maximum time permitted to wait for the process to finish
  *                    initializing; if the initialization heartbeat is not
  *                    received within this time the boot is treated as failed
  *                    and the process is gracefully stopped
  */
class LanguageServerProcess(
  progressTracker: ActorRef,
  descriptor: LanguageServerDescriptor,
  rpcPort: Int,
  dataPort: Int,
  bootTimeout: FiniteDuration
) extends Actor
    with Stash
    with RuntimeVersionManagerMixin {

  /** @inheritdoc */
  override def distributionConfiguration: DistributionConfiguration =
    descriptor.distributionConfiguration

  override def preStart(): Unit = {
    super.preStart()
    self ! Boot
  }

  override def receive: Receive = initializationStage

  /** First stage, it launches the child process and sets up the futures to send
    * back the notifications and goes to the startingStage.
    */
  private def initializationStage: Receive = {
    case Boot =>
      import context.dispatcher
      val (process, stopped) = launchProcessInBackground()
      context.become(startingStage)
      process.map(ProcessStarted) pipeTo self
      stopped.map(ServerTerminated).recover(ServerThreadFailed(_)) pipeTo self
    case _ => stash()
  }

  /** Starts the process in a background thread.
    *
    * It returns a pair of futures: first one is completed once the process has
    * actually been started and contains a handle for that process; the second
    * one is completed with the exit code once the process exits. The second
    * future may also be completed with an exception if the process cannot be
    * started, in such case the first future never completes.
    */
  private def launchProcessInBackground(): (Future[Process], Future[Int]) = {
    val processStartPromise = Promise[Process]()
    val stopped = Future {
      runServer(processStartPromise)
    }(LanguageServerProcess.forkedProcessExecutionContext)

    (processStartPromise.future, stopped)
  }

  /** Waits for the process to actually start (this may take a long time if
    * there are locked locks).
    *
    * Once the process is started, it goes to the bootingStage.
    */
  private def startingStage: Receive = {
    case ProcessStarted(process) =>
      import context.dispatcher
      val cancellable =
        context.system.scheduler.scheduleOnce(bootTimeout, self, TimedOut)
      context.become(bootingStage(process, cancellable))
      unstashAll()
      self ! AskServerIfStarted
    case ServerThreadFailed(error) => handleFatalError(error)
    case _                         => stash()
  }

  private def handleFatalError(error: Throwable): Unit = {
    context.parent ! ServerThreadFailed(error)
    context.stop(self)
  }

  /** In booting stage, the actor is retrying to connect to the server to verify
    * that it has finished initialization.
    *
    * Once initialization is confirmed, it notifies the parent and proceeds to
    * runningStage.
    *
    * Before initialization is confirmed the actor can also:
    * - trigger the timeout or receive a graceful stop message which will ask
    *   the child process to terminate gracefully,
    * - get a notification that the child process has terminated spuriously
    *   (possibly a crash).
    */
  private def bootingStage(
    process: Process,
    bootTimeout: Cancellable
  ): Receive =
    handleBootResponse(process, bootTimeout).orElse(runningStage(process))

  case object AskServerIfStarted
  case object TimedOut
  private val retryDelay = 100.milliseconds

  private def handleBootResponse(
    process: Process,
    bootTimeout: Cancellable
  ): Receive = {
    case AskServerIfStarted =>
      val socket = Socket(descriptor.networkConfig.interface, rpcPort)
      context.actorOf(
        HeartbeatSession.initialProps(
          socket,
          retryDelay,
          new AkkaBasedWebSocketConnectionFactory()(context.system),
          context.system.scheduler
        ),
        s"initial-heartbeat-${UUID.randomUUID()}"
      )
    case TimedOut =>
      self ! Stop
    case HeartbeatReceived =>
      context.parent ! LanguageServerProcess.ServerConfirmedFinishedBooting
      bootTimeout.cancel()
      context.become(runningStage(process))
    case ServerUnresponsive =>
      import context.dispatcher
      context.system.scheduler.scheduleOnce(
        retryDelay,
        self,
        AskServerIfStarted
      )
      context.become(bootingStage(process, bootTimeout))
  }

  /** When the process is running, the actor is monitoring it and managing its
    * lifetime.
    *
    * If termination is requested, the process will be asked to gracefully
    * terminate or be killed. If the process terminates (either on request or
    * spuriously, e.g. by a crash), the parent is notified and the current actor
    * is stopped.
    */
  private def runningStage(process: Process): Receive = {
    case Stop =>
      requestGracefulTermination(process)
    case Kill => process.destroyForcibly()
    case ServerTerminated(exitCode) =>
      context.parent ! ServerTerminated(exitCode)
      context.stop(self)
    case ServerThreadFailed(error) => handleFatalError(error)
  }

  /** Runs the child process, ensuring that the proper locks are kept for the
    * used engine for the whole lifetime of that process.
    *
    * Returns the exit code of the process. This function is blocking so it
    * should be run in a backgroung thread.
    */
  private def runServer(processStarted: Promise[Process]): Int = {
    val versionManager = makeRuntimeVersionManager(progressTracker)
    // TODO [RW] logging #1151
    val loggerConnection = Future.successful(None)
    val logLevel         = LogLevel.Info
    val options = LanguageServerOptions(
      rootId    = descriptor.rootId,
      interface = descriptor.networkConfig.interface,
      rpcPort   = rpcPort,
      dataPort  = dataPort
    )

    val runner = new Runner(
      versionManager,
      distributionConfiguration.environment,
      loggerConnection
    )
    val runSettings = runner
      .startLanguageServer(
        options             = options,
        projectPath         = descriptor.rootPath,
        version             = descriptor.engineVersion,
        logLevel            = logLevel,
        additionalArguments = Seq()
      )
      .get
    runner.withCommand(runSettings, descriptor.jvmSettings) { command =>
      val process = {
        val pb = command.builder()
        pb.inheritIO()

        pb.redirectInput(Redirect.PIPE)
        if (descriptor.discardOutput) {
          pb.redirectError(Redirect.DISCARD)
          pb.redirectOutput(Redirect.DISCARD)
        }

        pb.start()
      }

      processStarted.success(process)
      process.waitFor()
    }
  }

  /** Requests the child process to terminate gracefully by sending the
    * termination request to its standard input stream.
    */
  def requestGracefulTermination(process: Process): Unit =
    Using(new PrintWriter(process.getOutputStream)) { writer =>
      writer.println()
    }

  private case object Boot
  private case class ProcessStarted(process: Process)
}

object LanguageServerProcess {

  /** Creates a configuration object used to create a [[LanguageServerProcess]].
    *
    * @param progressTracker an [[ActorRef]] that will get progress updates
    *                        related to initializing the engine
    * @param descriptor a LS descriptor
    * @param rpcPort port to bind for RPC connections
    * @param dataPort port to bind for binary connections
    * @param bootTimeout maximum time permitted to wait for the process to finish
    *                    initializing; if the initialization heartbeat is not
    *                    received within this time the boot is treated as failed
    *                    and the process is gracefully stopped
    * @return a configuration object
    */
  def props(
    progressTracker: ActorRef,
    descriptor: LanguageServerDescriptor,
    rpcPort: Int,
    dataPort: Int,
    bootTimeout: FiniteDuration
  ): Props = Props(
    new LanguageServerProcess(
      progressTracker,
      descriptor,
      rpcPort,
      dataPort,
      bootTimeout
    )
  )

  /** Sent to the parent when the server has terminated (for any reason: on
    * request or on its own, e.g. due to a signal or crash).
    */
  case class ServerTerminated(exitCode: Int)

  /** Sent to the parent when starting the server has failed with an exception.
    */
  case class ServerThreadFailed(throwable: Throwable)

  /** Sent to the parent when the child process has confirmed that it is fully
    * initialized.
    */
  case object ServerConfirmedFinishedBooting

  /** Sent to forcibly kill the server. */
  case object Kill

  /** Sent to gracefully request to stop the server. */
  case object Stop

  /** An executor that creates a new Thread for each job.
    *
    * It is used when creating the Future that will run the process in a
    * background thread. It is blocking by design to ensure that the locking API
    * is used correctly. This executor should start no more than one thread per
    * Language Server instance.
    */
  private object ForkedProcessExecutor extends Executor {

    /** @inheritdoc */
    override def execute(command: Runnable): Unit = {
      val thread = new Thread(command)
      thread.start()
    }
  }

  /** [[ExecutionContext]] associated with the [[ForkedProcessExecutor]]. */
  val forkedProcessExecutionContext: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(ForkedProcessExecutor)
}
