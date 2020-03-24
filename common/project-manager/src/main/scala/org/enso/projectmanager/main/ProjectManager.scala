package org.enso.projectmanager.main

import java.io.IOException

import akka.http.scaladsl.Http
import com.typesafe.scalalogging.LazyLogging
import org.enso.projectmanager.main.Globals.{
  ConfigFilename,
  ConfigNamespace,
  FailureExitCode,
  SuccessExitCode
}
import org.enso.projectmanager.main.configuration.ProjectManagerConfig
import pureconfig.ConfigSource
import zio.ZIO.effectTotal
import zio._
import zio.console._

import scala.concurrent.Await
import scala.concurrent.duration._
import pureconfig.ConfigSource
import org.enso.projectmanager.infrastructure.config.ConfigurationReaders.fileReader
import pureconfig._
import pureconfig.generic.auto._
import zio._
import zio.interop.catz.core._

/**
  * Project manager runner containing the main method.
  */
object ProjectManager extends App with LazyLogging {

  logger.info("Starting Language Server...")

  /**
    * A configuration of the project manager.
    */
  val config: ProjectManagerConfig =
    ConfigSource
      .resources(ConfigFilename)
      .withFallback(ConfigSource.systemProperties)
      .at(ConfigNamespace)
      .loadOrThrow[ProjectManagerConfig]

  /**
    * ZIO runtime.
    */
  implicit val runtime = Runtime.default

  /**
    * Main process starting up the server.
    */
  lazy val mainProcess: ZIO[ZEnv, IOException, Unit] = {
    val mainModule = new MainModule[ZIO[ZEnv, +*, +*]](config)
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
    mainProcess.fold(_ => FailureExitCode, _ => SuccessExitCode)

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
