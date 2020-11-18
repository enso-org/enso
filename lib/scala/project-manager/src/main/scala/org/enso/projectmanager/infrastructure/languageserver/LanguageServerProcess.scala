package org.enso.projectmanager.infrastructure.languageserver
import java.io.PrintWriter
import java.lang.ProcessBuilder.Redirect
import java.util.concurrent.TimeUnit

import akka.pattern.pipe
import akka.actor.{Actor, ActorRef, Stash}
import nl.gn0s1s.bump.SemVer
import org.enso.loggingservice.LogLevel
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
import org.enso.runtimeversionmanager.runner.{
  JVMSettings,
  LanguageServerOptions,
  Runner
}

import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise, TimeoutException}
import scala.util.Using

class LanguageServerProcess(
  override val distributionConfiguration: DistributionConfiguration,
  version: SemVer,
  projectPath: String,
  progressTracker: ActorRef,
  languageServerOptions: LanguageServerOptions,
  jvmSettings: JVMSettings,
  bootTimeout: FiniteDuration
) extends Actor
    with Stash
    with RuntimeVersionManagerMixin {

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
      Future { waitForStartup() } pipeTo context.parent
      // FIXME [RW] start cancellable for boot timeout
      context.become(bootingStage(process))
    case _ => stash()
  }

  private def bootingStage(process: Process): Receive =
    handleBootResponse(process).orElse(runningStage(process))

  private def handleBootResponse(process: Process): Receive = {
    case HeartbeatReceived =>
      // TODO confirm
      context.become(runningStage(process))
    case ServerUnresponsive =>
    // TODO retry? maybe wait a little bit
  }

  // TODO [RW] figure out how to deal with stopping the process
  //  (preferably gracefully when this Actor is stopped)
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

    val runner =
      new Runner(mgr, distributionConfiguration.environment, loggerConnection)
    val runSettings = runner
      .startLanguageServer(
        options             = languageServerOptions,
        projectPath         = projectPath,
        version             = version,
        logLevel            = logLevel,
        additionalArguments = Seq()
      )
      .get
    runner.withCommand(runSettings, jvmSettings) { command =>
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

  /** Waits until the server has processStarted.
    *
    * It first waits for the server to actually start, not consuming the timeout
    * while the server is being initialized. Once it is notified that the
    * process is started, it tries to connect to the server to ensure that it is
    * processStarted. It retries until the timeout is reached.
    */
  private def waitForStartup(): Unit = {
    val deadline = Deadline.now + bootTimeout
    var booted   = false
    while (!booted && deadline.hasTimeLeft()) {
      if (queryServer()) {
        booted = true
      }
    }

    if (!booted)
      throw new TimeoutException(
        "The server did not finish booting within the requested timeout."
      )
  }

  private def queryServer(): Boolean = {
    ??? // FIXME [RW]
  }
}

object LanguageServerProcess {
  def props(): Nothing = ???

  /** Sent to the parent when the server has terminated (for any reason: on
    * request or on its own, e.g. due to a signal or crash).
    */
  case class ServerTerminated(exitCode: Int)

  /** Sent to forcibly kill the server. */
  case object Kill

  /** Sent to gracefully request to stop the server. */
  case object Stop

}
