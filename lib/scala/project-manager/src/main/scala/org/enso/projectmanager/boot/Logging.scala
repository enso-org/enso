package org.enso.projectmanager.boot

import java.nio.file.Path
import org.enso.projectmanager.service.LoggingServiceDescriptor
import org.enso.projectmanager.versionmanagement.DefaultDistributionConfiguration
import org.slf4j.event.Level

import java.net.URI

import scala.concurrent.Future
import org.enso.logging.LoggingSetupHelper
import scala.concurrent.ExecutionContext.Implicits.global

/** A helper for setting up the logging service in the Project Manager. */
object Logging extends LoggingSetupHelper(global) {

  /** @inheritdoc */
  override val defaultLogLevel: Level = Level.INFO

  /** @inheritdoc */
  override lazy val logPath: Path =
    DefaultDistributionConfiguration.distributionManager.paths.logs

  /** @inheritdoc */
  override val logFileSuffix: String = "enso-project-manager"

  object GlobalLoggingService extends LoggingServiceDescriptor {

    /** @inheritdoc */
    override def getEndpoint: Future[Option[URI]] = loggingServiceEndpoint()
  }
}
