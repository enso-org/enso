package org.enso.launcher.cli

import java.nio.file.Path

import org.enso.launcher.distribution.DefaultManagers
import org.enso.loggingservice.{
  ColorMode,
  LogLevel,
  LoggingServiceManager,
  LoggingServiceSetupHelper
}

import scala.concurrent.ExecutionContext.Implicits.global

/** Manages setting up the logging service within the launcher.
  */
object LauncherLogging extends LoggingServiceSetupHelper {

  /** @inheritdoc */
  override val defaultLogLevel: LogLevel = LogLevel.Warning

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
  def prepareForUninstall(colorMode: ColorMode): Unit = {
    waitForSetup()
    LoggingServiceManager.replaceWithFallback(printers =
      Seq(stderrPrinter(colorMode, printExceptions = true))
    )
  }
}
