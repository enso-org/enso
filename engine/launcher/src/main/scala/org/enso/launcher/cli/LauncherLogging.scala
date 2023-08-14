package org.enso.launcher.cli

import java.nio.file.Path
import org.enso.launcher.distribution.DefaultManagers
import org.slf4j.event.Level

import org.enso.logging.LoggingCollectorHelper
import org.enso.logging.LoggingServiceManager

import scala.concurrent.ExecutionContext.Implicits.global

/** Manages setting up the logging service within the launcher.
  */
object LauncherLogging extends LoggingCollectorHelper {

  /** @inheritdoc */
  override val defaultLogLevel: Level = Level.WARN

  /** @inheritdoc */
  override val logFileSuffix: String = "enso-launcher"

  /** @inheritdoc */
  override lazy val logPath: Path =
    DefaultManagers.distributionManager.paths.logs

  override val logComponentName: String = "launcher"

  /** Turns off the main logging service, falling back to just a stderr backend.
    *
    * This method should be called as part of uninstalling the distribution. The
    * server can be safely shutdown as during uninstallation no other components
    * should be running.
    *
    * This is necessary on Windows to ensure that the logs file is closed, so
    * that the log directory can be removed.
    */
  def prepareForUninstall(): Unit = { //colorMode: ColorMode): Unit = {
    // TODO
    waitForSetup()

    // TODO: fetch actual log level
    LoggingServiceManager.fallbackToLocalConsole(defaultLogLevel, "launcher")
  }
}
