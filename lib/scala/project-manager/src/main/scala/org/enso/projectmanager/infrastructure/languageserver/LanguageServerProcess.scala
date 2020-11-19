package org.enso.projectmanager.infrastructure.languageserver
import java.io.PrintWriter
import java.lang.ProcessBuilder.Redirect
import java.util.UUID

import akka.actor.{Actor, ActorRef, Cancellable, Props, Stash}
import akka.pattern.pipe
import org.enso.loggingservice.LogLevel
import org.enso.projectmanager.data.Socket
import org.enso.projectmanager.infrastructure.http.AkkaBasedWebSocketConnectionFactory
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProcess.{
  Kill,
  ServerTerminated,
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
import scala.concurrent.{Future, Promise}
import scala.util.Using

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

  import context.dispatcher

  override def receive: Receive = initializationStage

  /** Sent by the Actor itself to start the boot process.
    *
    * For internal use only.
    */
  case object Boot

  case class ProcessStarted(process: Process)

  private def initializationStage: Receive = {
    case Boot =>
      val processStartPromise = Promise[Process]()
      val stopped = Future {
        runServer(processStartPromise)
      }.map(ServerTerminated)

      processStartPromise.future.map(ProcessStarted) pipeTo self
      stopped pipeTo self
      context.become(startingStage)
    case _ => stash()
  }

  private def startingStage: Receive = {
    case ProcessStarted(process) =>
      self ! AskServerIfStarted
      val cancellable =
        context.system.scheduler.scheduleOnce(bootTimeout, self, TimedOut)
      context.become(bootingStage(process, cancellable))
    case _ => stash()
  }

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
      context.system.scheduler.scheduleOnce(
        retryDelay,
        self,
        AskServerIfStarted
      )
      context.become(bootingStage(process, bootTimeout))
  }

  // TODO restarting, retrying
  private def runningStage(process: Process): Receive = {
    case Stop => requestGracefulTermination(process)
    case Kill => process.destroyForcibly()
    case ServerTerminated(exitCode) =>
      context.parent ! ServerTerminated(exitCode)
      context.stop(self)
  }

  private def runServer(processStarted: Promise[Process]): Int = {
    val mgr = makeRuntimeVersionManager(progressTracker)
    // TODO [RW] logging #1151
    val loggerConnection = Future.successful(None)
    val logLevel         = LogLevel.Info
    val options = LanguageServerOptions(
      rootId    = descriptor.rootId,
      interface = descriptor.networkConfig.interface,
      rpcPort   = rpcPort,
      dataPort  = dataPort
    )

    val runner =
      new Runner(mgr, distributionConfiguration.environment, loggerConnection)
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
        pb.start()
      }

      processStarted.success(process)
      process.waitFor()
    }
  }

  // TODO [RW] we may want to use a socket to send this termination message?
  def requestGracefulTermination(process: Process): Unit =
    Using(new PrintWriter(process.getOutputStream)) { writer =>
      writer.println()
    }
}

object LanguageServerProcess {
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

  case object ServerConfirmedFinishedBooting

  /** Sent to forcibly kill the server. */
  case object Kill

  /** Sent to gracefully request to stop the server. */
  case object Stop

}
