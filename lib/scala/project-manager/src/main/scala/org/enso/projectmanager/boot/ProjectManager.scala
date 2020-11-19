package org.enso.projectmanager.boot

import java.io.IOException
import java.util.concurrent.ScheduledThreadPoolExecutor

import akka.http.scaladsl.Http
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.cli.CommandLine
import org.enso.loggingservice.LogLevel
import org.enso.projectmanager.boot.Globals.{
  ConfigFilename,
  ConfigNamespace,
  FailureExitCode,
  SuccessExitCode
}
import org.enso.projectmanager.boot.configuration.ProjectManagerConfig
import org.enso.version.VersionDescription
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.ZIO.effectTotal
import zio._
import zio.console._
import zio.interop.catz.core._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor}

/** Project manager runner containing the main method.
  */
object ProjectManager extends App with LazyLogging {

  /** A configuration of the project manager.
    */
  val config: ProjectManagerConfig =
    ConfigSource
      .resources(ConfigFilename)
      .withFallback(ConfigSource.systemProperties)
      .at(ConfigNamespace)
      .loadOrThrow[ProjectManagerConfig]

  val computeThreadPool = new ScheduledThreadPoolExecutor(
    java.lang.Runtime.getRuntime.availableProcessors()
  )

  val computeExecutionContext: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(
      computeThreadPool,
      th => logger.error("An expected error occurred", th)
    )

  /** ZIO runtime.
    */
  implicit val runtime =
    Runtime(Globals.zioEnvironment, new ZioPlatform(computeExecutionContext))

  /** Main process starting up the server.
    */
  lazy val mainProcess: ZIO[ZEnv, IOException, Unit] = {
    val mainModule =
      new MainModule[ZIO[ZEnv, +*, +*]](config, computeExecutionContext)
    for {
      binding <- bindServer(mainModule)
      _       <- logServerStartup()
      _       <- getStrLn
      _       <- effectTotal { logger.info("Stopping server...") }
      _       <- effectTotal { binding.unbind() }
      _       <- killAllLanguageServer(mainModule)
      _       <- waitTillAllShutdownHooksWillBeFired(mainModule)
      _       <- effectTotal { mainModule.system.terminate() }
    } yield ()
  }

  private def killAllLanguageServer(mainModule: MainModule[ZIO[ZEnv, +*, +*]]) =
    mainModule.languageServerGateway
      .killAllServers()
      .foldM(
        failure = th =>
          effectTotal {
            logger.error("An error occurred during killing lang servers", th)
          },
        success = ZIO.succeed(_)
      )

  private def waitTillAllShutdownHooksWillBeFired(
    mainModule: MainModule[ZIO[ZEnv, +*, +*]]
  ) =
    mainModule.languageServerGateway
      .waitTillAllHooksFired()
      .foldM(
        failure = th =>
          effectTotal {
            logger
              .error("An error occurred during waiting for shutdown hooks", th)
          },
        success = ZIO.succeed(_)
      )

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    Cli.parse(args.toArray) match {
      case Right(opts) =>
        runOpts(opts)
      case Left(error) =>
        putStrLn(error) *>
        effectTotal(Cli.printHelp()) *>
        ZIO.succeed(FailureExitCode)
    }
  }

  /** The main function of the application, which will be passed the command-line
    * arguments to the program and has to return an `IO` with the errors fully handled.
    */
  def runOpts(options: CommandLine): ZIO[ZEnv, Nothing, ExitCode] = {
    if (options.hasOption(Cli.HELP_OPTION)) {
      ZIO.effectTotal(Cli.printHelp()) *>
      ZIO.succeed(SuccessExitCode)
    } else if (options.hasOption(Cli.VERSION_OPTION)) {
      displayVersion(options.hasOption(Cli.JSON_OPTION))
    } else {
      val verbosity = options.getOptions.count(_ == Cli.option.verbose)
      logger.info("Starting Project Manager...")
      setupLogging(verbosity) *>
      mainProcess.fold(
        th => { th.printStackTrace(); FailureExitCode },
        _ => SuccessExitCode
      )
    }
  }

  private def setupLogging(verbosityLevel: Int): ZIO[Console, Nothing, Unit] = {
    val level = verbosityLevel match {
      case 0 => LogLevel.Info
      case 1 => LogLevel.Debug
      case _ => LogLevel.Trace
    }
    ZIO
      .fromFuture(executionContext => Logging.setup(level, executionContext))
      .catchAll { exception =>
        putStrLnErr(s"Failed to setup the logger: $exception")
      }
  }

  private def displayVersion(
    useJson: Boolean
  ): ZIO[Console, Nothing, ExitCode] = {
    val versionDescription = VersionDescription.make(
      "Enso Project Manager",
      includeRuntimeJVMInfo = true
    )
    putStrLn(versionDescription.asString(useJson)) *>
    ZIO.succeed(SuccessExitCode)
  }

  private def logServerStartup(): UIO[Unit] =
    effectTotal {
      logger.info(
        s"Started server at ${config.server.host}:${config.server.port}, press enter to kill server"
      )
    }

  private def bindServer(
    module: MainModule[ZIO[ZEnv, +*, +*]]
  ): UIO[Http.ServerBinding] =
    effectTotal {
      Await.result(
        module.server.bind(config.server.host, config.server.port),
        3.seconds
      )
    }

}
