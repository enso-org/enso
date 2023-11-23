package org.enso.projectmanager.infrastructure.languageserver
import java.util.UUID

import akka.actor.{Actor, ActorRef, Cancellable, Props, Stash}
import org.enso.projectmanager.data.Socket
import org.enso.projectmanager.infrastructure.http.AkkaBasedWebSocketConnectionFactory
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerExecutor.ProcessHandle
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

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** An Actor that manages a single Language Server process.
  *
  * It starts the process upon creation and notifies the parent once the process
  * has finished booting.
  *
  * @param progressTracker an [[ActorRef]] that will get progress updates
  *                        related to initializing the engine
  * @param descriptor a LS descriptor
  * @param rpcPort port to bind for RPC connections
  * @param secureRpcPort an optional port to bind for secure RPC connections
  * @param dataPort port to bind for binary connections
  * @param secureDataPort an optional port to bind for secure  binary connections
  * @param bootTimeout maximum time permitted to wait for the process to finish
  *                    initializing; if the initialization heartbeat is not
  *                    received within this time the boot is treated as failed
  *                    and the process is gracefully stopped
  * @param executor an executor service used to start the language server
  *                 process
  */
class LanguageServerProcess(
  progressTracker: ActorRef,
  descriptor: LanguageServerDescriptor,
  rpcPort: Int,
  secureRpcPort: Option[Int],
  dataPort: Int,
  secureDataPort: Option[Int],
  bootTimeout: FiniteDuration,
  executor: LanguageServerExecutor
) extends Actor
    with Stash {

  import context.dispatcher

  override def preStart(): Unit = {
    super.preStart()
    self ! Boot
  }

  override def receive: Receive = initializationStage

  object LifecycleListener extends LanguageServerExecutor.LifecycleListener {
    override def onStarted(processHandle: ProcessHandle): Unit =
      self ! ProcessStarted(processHandle)

    override def onTerminated(exitCode: Int): Unit =
      self ! ProcessTerminated(exitCode)

    override def onFailed(throwable: Throwable): Unit =
      self ! ProcessFailed(throwable)
  }

  /** First stage, it launches the child process and sets up the futures to send
    * back the notifications and goes to the startingStage.
    */
  private def initializationStage: Receive = {
    case Boot =>
      executor.spawn(
        descriptor        = descriptor,
        progressTracker   = progressTracker,
        rpcPort           = rpcPort,
        secureRpcPort     = secureRpcPort,
        dataPort          = dataPort,
        secureDataPort    = secureDataPort,
        lifecycleListener = LifecycleListener
      )
      context.become(startingStage)
    case _ => stash()
  }

  /** Waits for the process to actually start (this may take a long time if
    * there are locked locks).
    *
    * Once the process is started, it goes to the bootingStage.
    */
  private def startingStage: Receive = {
    case ProcessStarted(process) =>
      val cancellable =
        context.system.scheduler.scheduleOnce(bootTimeout, self, TimedOut)
      context.become(bootingStage(process, cancellable))
      unstashAll()
      self ! AskServerIfStarted
    case ProcessFailed(error) => handleFatalError(error)
    case _                    => stash()
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
    process: ProcessHandle,
    bootTimeout: Cancellable
  ): Receive =
    handleBootResponse(process, bootTimeout).orElse(runningStage(process))

  case object AskServerIfStarted
  case object TimedOut
  private val retryDelay = 100.milliseconds

  private def handleBootResponse(
    process: ProcessHandle,
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
  private def runningStage(process: ProcessHandle): Receive = {
    case Stop => process.requestGracefulTermination()
    case Kill => process.kill()
    case ProcessTerminated(exitCode) =>
      context.parent ! ServerTerminated(exitCode)
      context.stop(self)
    case ProcessFailed(error) => handleFatalError(error)
  }

  private case object Boot
  private case class ProcessStarted(process: ProcessHandle)
  private case class ProcessTerminated(exitCode: Int)
  private case class ProcessFailed(throwable: Throwable)
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
    * @param executor an executor service used to start the language server
    *                 process
    * @return a configuration object
    */
  def props(
    progressTracker: ActorRef,
    descriptor: LanguageServerDescriptor,
    rpcPort: Int,
    secureRpcPort: Option[Int],
    dataPort: Int,
    secureDataPort: Option[Int],
    bootTimeout: FiniteDuration,
    executor: LanguageServerExecutor
  ): Props = Props(
    new LanguageServerProcess(
      progressTracker,
      descriptor,
      rpcPort,
      secureRpcPort,
      dataPort,
      secureDataPort,
      bootTimeout,
      executor
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
}
