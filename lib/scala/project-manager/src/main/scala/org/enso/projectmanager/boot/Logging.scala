package org.enso.projectmanager.boot

import java.nio.file.Path

import org.enso.loggingservice.{LogLevel, LoggingServiceSetupHelper}
import org.enso.projectmanager.versionmanagement.DefaultDistributionConfiguration

import scala.concurrent.ExecutionContext.Implicits.global

/** A helper for setting up the logging service in the Project Manager. */
object Logging extends LoggingServiceSetupHelper {

  /** @inheritdoc */
  override val defaultLogLevel: LogLevel = LogLevel.Info

  /** @inheritdoc */
  override lazy val logPath: Path =
    DefaultDistributionConfiguration.distributionManager.paths.logs

  /** @inheritdoc */
  override val logFileSuffix: String = "enso-project-manager"
}
