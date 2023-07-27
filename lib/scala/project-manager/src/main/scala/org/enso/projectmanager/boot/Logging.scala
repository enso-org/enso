package org.enso.projectmanager.boot

import java.nio.file.Path
import org.enso.projectmanager.service.LoggingServiceDescriptor
import org.enso.projectmanager.versionmanagement.DefaultDistributionConfiguration
import org.slf4j.event.Level

import java.net.URI

import scala.concurrent.Future
import org.enso.logging.LoggingCollectorHelper
import scala.concurrent.ExecutionContext.Implicits.global

/** A helper for setting up the logging service in the Project Manager. */
object Logging extends LoggingCollectorHelper {

  /** @inheritdoc */
  override val defaultLogLevel: Level = Level.INFO

  /** @inheritdoc */
  override lazy val logPath: Path =
    DefaultDistributionConfiguration.distributionManager.paths.logs

  /** @inheritdoc */
  override val logFileSuffix: String = "enso-project-manager"

  override def logComponentName: String = "project-manager"
  //override def defaultAppenderName: Option[String] = Some("forward-via-socket")

  object GlobalLoggingService extends LoggingServiceDescriptor {

    /** @inheritdoc */
    override def getEndpoint: Future[Option[URI]] = loggingServiceEndpoint()
  }
}
