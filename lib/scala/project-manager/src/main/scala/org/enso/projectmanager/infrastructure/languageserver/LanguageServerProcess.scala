package org.enso.projectmanager.infrastructure.languageserver
import java.io.PrintWriter
import java.lang.ProcessBuilder.Redirect
import java.util.concurrent.{Semaphore, TimeUnit}

import akka.actor.ActorRef
import nl.gn0s1s.bump.SemVer
import org.enso.loggingservice.LogLevel
import org.enso.projectmanager.data.MissingComponentAction
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagerMixin
import org.enso.projectmanager.versionmanagement.DistributionConfiguration
import org.enso.runtimeversionmanager.runner.{
  JVMSettings,
  LanguageServerOptions,
  Runner
}

import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, TimeoutException}
import scala.util.Using

class LanguageServerProcess(
  override val distributionConfiguration: DistributionConfiguration,
  version: SemVer,
  projectPath: String,
  progressTracker: ActorRef,
  missingComponentAction: MissingComponentAction,
  languageServerOptions: LanguageServerOptions,
  jvmSettings: JVMSettings,
  bootTimeout: FiniteDuration
)(implicit executionContext: ExecutionContext)
    extends LifecycleComponent2
    with RuntimeVersionManagerMixin {
  var serverRunning: Option[Future[Int]] = None
  var currentProcess: Option[Process]    = None

  val startSemaphore = new Semaphore(0)
  val stopSemaphore  = new Semaphore(0)

  override def start(): Future[Unit] = {
    if (serverRunning.isDefined) {
      Future.failed(new IllegalStateException("Server is already running"))
    } else {
      serverRunning = Some(Future(runServer()))
      Future(waitForStartup())
    }
  }

  private def runServer(): Int = {
    val mgr = makeRuntimeVersionManager(progressTracker, missingComponentAction)
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

      startSemaphore.release()
      currentProcess = Some(process)
      var stopRequested = false
      while (!stopRequested && process.isAlive) {
        val acquired = stopSemaphore.tryAcquire(200, TimeUnit.MILLISECONDS)
        if (acquired) {
          stopRequested = true
        }
      }

      // TODO [RW] we may want to use a socket to send this termination message?
      def requestTermination(): Unit =
        Using(new PrintWriter(process.getOutputStream)) { writer =>
          writer.println()
        }

      if (stopRequested) {
        requestTermination()
      }

      process.waitFor()
    }
  }

  /** Waits until the server has booted.
    *
    * It first waits for the server to actually start, not consuming the timeout
    * while the server is being initialized. Once it is notified that the
    * process is started, it tries to connect to the server to ensure that it is
    * booted. It retries until the timeout is reached.
    */
  private def waitForStartup(): Unit = {
    stopSemaphore.acquire()
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

  override def stop(): Future[Int] =
    serverRunning match {
      case Some(value) =>
        stopSemaphore.release()
        value
      case None =>
        Future.failed(new IllegalStateException("Server was not running"))
    }

  def kill(): Future[Int] = {
    (serverRunning, currentProcess) match {
      case (Some(finished), Some(proc)) =>
        proc.destroyForcibly()
        finished
      case _ =>
        Future.failed(new IllegalStateException("Server was not running"))
    }
  }

  override def restart(): Future[Unit] = for {
    _ <- stop()
    _ <- start()
  } yield ()
}
