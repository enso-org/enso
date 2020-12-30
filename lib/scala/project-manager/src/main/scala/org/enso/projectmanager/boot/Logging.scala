package org.enso.projectmanager.boot

import java.nio.file.Path

import akka.http.scaladsl.model.Uri
import org.enso.loggingservice.{LogLevel, LoggingServiceSetupHelper}
import org.enso.projectmanager.service.LoggingServiceDescriptor
import org.enso.projectmanager.versionmanagement.DefaultDistributionConfiguration

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/** A helper for setting up the logging service in the Project Manager. */
object Logging extends LoggingServiceSetupHelper {

  /** @inheritdoc */
  override val defaultLogLevel: LogLevel = LogLevel.Info

  /** @inheritdoc */
  override lazy val logPath: Path =
    DefaultDistributionConfiguration.distributionManager.paths.logs

  /** @inheritdoc */
  override val logFileSuffix: String = "enso-project-manager"

  object GlobalLoggingService extends LoggingServiceDescriptor {

    /** @inheritdoc */
    override def getEndpoint: Future[Option[Uri]] = loggingServiceEndpoint()
  }
}
