package org.enso.projectmanager.infrastructure.languageserver

import java.lang.ProcessBuilder.Redirect
import java.util.concurrent.Executors

import akka.actor.ActorRef
import org.apache.commons.lang3.concurrent.BasicThreadFactory
import org.enso.loggingservice.LogLevel
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerExecutor.SpawnResult
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagerMixin
import org.enso.projectmanager.versionmanagement.DistributionConfiguration
import org.enso.runtimeversionmanager.runner.{LanguageServerOptions, Runner}

import scala.concurrent.{
  ExecutionContext,
  ExecutionContextExecutor,
  Future,
  Promise
}

/** An executor that allows to start the language server process.
  *
  * @param descriptor options related to this language server instance
  * @param progressTracker reference to an actor that should be notifed of any
  *                        locks
  * @param rpcPort port to use for the RPC channel
  * @param dataPort port to use for the binary channel
  */
class LanguageServerExecutor(
  descriptor: LanguageServerDescriptor,
  progressTracker: ActorRef,
  rpcPort: Int,
  dataPort: Int
) extends RuntimeVersionManagerMixin {

  /** @inheritdoc */
  override def distributionConfiguration: DistributionConfiguration =
    descriptor.distributionConfiguration

  /** Starts the process in a background thread. */
  def spawn(): SpawnResult = {
    val processStartPromise = Promise[LanguageServerProcessHandle]()
    val stopped = Future {
      runServer(processStartPromise)
    }(LanguageServerExecutor.forkedProcessExecutionContext)

    SpawnResult(processStartPromise.future, stopped)
  }

  /** Runs the child process, ensuring that the proper locks are kept for the
    * used engine for the whole lifetime of that process.
    *
    * Returns the exit code of the process. This function is blocking so it
    * should be run in a backgroung thread.
    */
  private def runServer(
    processStarted: Promise[LanguageServerProcessHandle]
  ): Int = {
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

      processStarted.success(new LanguageServerProcessHandle(process))
      process.waitFor()
    }
  }
}

object LanguageServerExecutor {

  /** Result of spawning a process.
    * @param processHandleFuture a future that is completed once the process has
    *                            actually been started and contains its handle
    * @param exitCodeFuture a future that is completed with the exit code once
    *                       the process exits; it may also be completed with an
    *                       exception if the process cannot be started, in this
    *                       case the first future never completes.
    */
  case class SpawnResult(
    processHandleFuture: Future[LanguageServerProcessHandle],
    exitCodeFuture: Future[Int]
  )

  /** Starts the language server process in a background thread.
    *
    * @param descriptor options related to this language server instance
    * @param progressTracker reference to an actor that should be notifed of any
    *                        locks
    * @param rpcPort port to use for the RPC channel
    * @param dataPort port to use for the binary channel
    * @return a pair of futures wrapped in a [[SpawnResult]]
    */
  def spawn(
    descriptor: LanguageServerDescriptor,
    progressTracker: ActorRef,
    rpcPort: Int,
    dataPort: Int
  ): SpawnResult = {
    val executor = new LanguageServerExecutor(
      descriptor      = descriptor,
      progressTracker = progressTracker,
      rpcPort         = rpcPort,
      dataPort        = dataPort
    )
    executor.spawn()
  }

  /** An execution context that ensures each job runs in a separate thread.
    *
    * It is used when creating the Future that will run the process in a
    * background thread. It is blocking by design to ensure that the locking API
    * is used correctly. This executor should start no more than one thread per
    * Language Server instance.
    */
  private val forkedProcessExecutionContext: ExecutionContextExecutor = {
    val threadFactory =
      new BasicThreadFactory.Builder()
        .namingPattern("language-server-fork-wrapper-%d")
        .build()
    val executor = Executors.newCachedThreadPool(threadFactory)
    ExecutionContext.fromExecutor(executor)
  }
}
