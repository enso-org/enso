package org.enso.projectmanager.infrastructure.languageserver

import java.io.PrintWriter
import java.lang.ProcessBuilder.Redirect
import java.util.concurrent.Executors

import akka.actor.ActorRef
import com.typesafe.scalalogging.Logger
import org.apache.commons.lang3.concurrent.BasicThreadFactory
import org.enso.loggingservice.LoggingServiceManager
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagerFactory
import org.enso.runtimeversionmanager.runner.{LanguageServerOptions, Runner}

import scala.util.Using

object ExecutorWithUnlimitedPool extends LanguageServerExecutor {

  /** An executor that ensures each job runs in a separate thread.
    *
    * It is used to run the process in a background thread. It is blocking by
    * design to ensure that the locking API is used correctly. This executor
    * should start no more than one thread per Language Server instance.
    */
  private val forkedProcessExecutor = {
    val threadFactory =
      new BasicThreadFactory.Builder()
        .namingPattern("language-server-pool-%d")
        .build()
    Executors.newCachedThreadPool(threadFactory)
  }

  /** @inheritdoc */
  override def spawn(
    descriptor: LanguageServerDescriptor,
    progressTracker: ActorRef,
    rpcPort: Int,
    dataPort: Int,
    lifecycleListener: LanguageServerExecutor.LifecycleListener
  ): Unit = {
    val runnable: Runnable = { () =>
      try {
        runServer(
          descriptor,
          progressTracker,
          rpcPort,
          dataPort,
          lifecycleListener
        )
      } catch {
        case throwable: Throwable =>
          lifecycleListener.onFailed(throwable)
      }
    }
    forkedProcessExecutor.submit(runnable)
  }

  /** Runs the child process, ensuring that the proper locks are kept for the
    * used engine for the whole lifetime of that process.
    *
    * Returns the exit code of the process. This function is blocking so it
    * should be run in a backgroung thread.
    */
  private def runServer(
    descriptor: LanguageServerDescriptor,
    progressTracker: ActorRef,
    rpcPort: Int,
    dataPort: Int,
    lifecycleListener: LanguageServerExecutor.LifecycleListener
  ): Unit = {
    val distributionConfiguration = descriptor.distributionConfiguration
    val versionManager = RuntimeVersionManagerFactory(distributionConfiguration)
      .makeRuntimeVersionManager(progressTracker)

    val inheritedLogLevel =
      LoggingServiceManager.currentLogLevelForThisApplication()
    val options = LanguageServerOptions(
      rootId    = descriptor.rootId,
      interface = descriptor.networkConfig.interface,
      rpcPort   = rpcPort,
      dataPort  = dataPort
    )

    val runner = new Runner(
      versionManager,
      distributionConfiguration.environment,
      descriptor.deferredLoggingServiceEndpoint
    )
    val runSettings = runner
      .startLanguageServer(
        options             = options,
        projectPath         = descriptor.rootPath,
        version             = descriptor.engineVersion,
        logLevel            = inheritedLogLevel,
        additionalArguments = Seq()
      )
      .get
    runner.withCommand(runSettings, descriptor.jvmSettings) { command =>
      Logger[ExecutorWithUnlimitedPool.type].trace(
        "Starting Language Server Process [{}]",
        command
      )

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

      lifecycleListener.onStarted(new LanguageServerProcessHandle(process))
      val exitCode = process.waitFor()
      lifecycleListener.onTerminated(exitCode)
    }
  }

  private class LanguageServerProcessHandle(private val process: Process)
      extends LanguageServerExecutor.ProcessHandle {

    /** Requests the child process to terminate gracefully by sending the
      * termination request to its standard input stream.
      */
    def requestGracefulTermination(): Unit =
      Using(new PrintWriter(process.getOutputStream)) { writer =>
        writer.println()
      }

    /** @inheritdoc */
    def kill(): Unit = {
      process.destroyForcibly()
    }
  }
}
