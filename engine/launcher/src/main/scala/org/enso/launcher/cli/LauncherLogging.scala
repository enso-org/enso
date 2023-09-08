package org.enso.launcher.cli

import java.nio.file.Path
import org.enso.launcher.distribution.DefaultManagers
import org.enso.logger.LoggerSetup
import org.slf4j.event.Level
import org.enso.logging.LoggingSetupHelper
import scala.concurrent.ExecutionContext.Implicits.global

/** Manages setting up the logging service within the launcher.
  */
object LauncherLogging extends LoggingSetupHelper(global) {

  /** @inheritdoc */
  override val defaultLogLevel: Level = Level.WARN

  /** @inheritdoc */
  override val logFileSuffix: String = "enso-launcher"

  /** @inheritdoc */
  override lazy val logPath: Path =
    DefaultManagers.distributionManager.paths.logs

  /** Turns off the main logging service, falling back to just a stderr backend.
    *
    * This method should be called as part of uninstalling the distribution. The
    * server can be safely shutdown as during uninstallation no other components
    * should be running.
    *
    * This is necessary on Windows to ensure that the logs file is closed, so
    * that the log directory can be removed.
    */
  def prepareForUninstall(logLevel: Option[Level]): Unit = {
    waitForSetup()
    val actualLogLevel = logLevel.getOrElse(defaultLogLevel)
    LoggerSetup.get().setupConsoleAppender(actualLogLevel)
  }
}
