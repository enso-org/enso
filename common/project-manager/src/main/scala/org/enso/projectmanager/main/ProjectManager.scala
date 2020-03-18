package org.enso.projectmanager.main

import com.typesafe.scalalogging.LazyLogging
import org.enso.projectmanager.main.configuration.ProjectManagerConfig
import pureconfig.ConfigSource

import scala.concurrent.Await
import scala.io.StdIn
import scala.concurrent.duration._
import pureconfig.generic.auto._

/**
  * Project manager runner containing the main method.
  */
object ProjectManager extends App with LazyLogging {

  logger.info("Starting Language Server...")

  val config: ProjectManagerConfig =
    ConfigSource
      .resources("application.conf")
      .withFallback(ConfigSource.systemProperties)
      .at("project-manager")
      .loadOrThrow[ProjectManagerConfig]

  val mainModule = new MainModule(config)

  val binding =
    Await.result(
      mainModule.server.bind(config.server.host, config.server.port),
      3.seconds
    )

  logger.info(
    s"Started server at ${config.server.host}:${config.server.port}, press enter to kill server"
  )
  StdIn.readLine()
  logger.info("Stopping server...")

  binding.unbind()
  mainModule.system.terminate()

}
