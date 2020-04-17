package org.enso.projectmanager.boot

import java.io.IOException
import java.util.concurrent.ScheduledThreadPoolExecutor

import akka.http.scaladsl.Http
import com.typesafe.scalalogging.LazyLogging
import org.enso.projectmanager.boot.Globals.{
  ConfigFilename,
  ConfigNamespace,
  FailureExitCode,
  SuccessExitCode
}
import org.enso.projectmanager.boot.configuration.ProjectManagerConfig
import pureconfig.ConfigSource
import zio.ZIO.effectTotal
import zio._
import zio.console._
import zio.interop.catz.core._
import org.enso.projectmanager.infrastructure.config.ConfigurationReaders.fileReader
import pureconfig._
import pureconfig.generic.auto._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor}

/**
  * Project manager runner containing the main method.
  */
object ProjectManager extends App with LazyLogging {

  logger.info("Starting Project Manager...")

  /**
    * A configuration of the project manager.
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

  /**
    * ZIO runtime.
    */
  implicit val runtime =
    Runtime(Globals.zioEnvironment, new ZioPlatform(computeExecutionContext))

  /**
    * Main process starting up the server.
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
      _       <- effectTotal { mainModule.system.terminate() }
    } yield ()
  }

  /**
    * The main function of the application, which will be passed the command-line
    * arguments to the program and has to return an `IO` with the errors fully handled.
    */
  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    mainProcess.fold(
      th => { th.printStackTrace(); FailureExitCode },
      _ => SuccessExitCode
    )

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
