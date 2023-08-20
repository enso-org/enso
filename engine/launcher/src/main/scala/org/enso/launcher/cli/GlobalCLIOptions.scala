package org.enso.launcher.cli

import org.enso.launcher.cli.GlobalCLIOptions.InternalOptions

import java.net.URI
import org.slf4j.event.Level

/** Gathers settings set by the global CLI options.
  *
  * @param autoConfirm if this flag is set, the program should not ask the user
  *                    any questions but proceed with the default values, that
  *                    must be explained in the help text for each command
  * @param hideProgress if this flag is set, progress bars should not be
  *                     printed
  * @param useJSON specifies if output should be in JSON format, if it is
  *                supported (currently only the version command supports JSON)
  * @param internalOptions options that are remembered to pass them to launcher
  *                        child processes
  */
case class GlobalCLIOptions(
  autoConfirm: Boolean,
  hideProgress: Boolean,
  useJSON: Boolean,
  internalOptions: InternalOptions
)

object GlobalCLIOptions {
  val HIDE_PROGRESS = "hide-progress"
  val AUTO_CONFIRM  = "auto-confirm"
  val USE_JSON      = "json"

  /** Internal options that are remembered to pass them to launcher child
    * processes.
    */
  case class InternalOptions(
    launcherLogLevel: Option[Level],
    loggerConnectUri: Option[URI],
    logMaskingDisabled: Boolean
  ) {

    /** @return `true` if log masking is enabled. */
    def logMasking: Boolean = !logMaskingDisabled

    /** Creates command line options that can be passed to a launcher process to
      * set the same options.
      */
    def toOptions: Seq[String] = {
      val level = launcherLogLevel
        .map(level => Seq(s"--$LOG_LEVEL", level.name))
        .getOrElse(Seq())
      val uri = loggerConnectUri
        .map(uri => Seq(s"--$CONNECT_LOGGER", uri.toString))
        .getOrElse(Seq())
      val noMasking =
        if (logMaskingDisabled) Seq(s"--$NO_LOG_MASKING") else Seq()
      level ++ uri ++ noMasking
    }
  }

  val LOG_LEVEL      = "launcher-log-level"
  val CONNECT_LOGGER = "internal-connect-logger"
  val NO_LOG_MASKING = "no-log-masking"

  /** Converts the [[GlobalCLIOptions]] to a sequence of arguments that can be
    * added to a launcher invocation to set the same options.
    */
  def toOptions(config: GlobalCLIOptions): Seq[String] = {
    val autoConfirm = if (config.autoConfirm) Seq(s"--$AUTO_CONFIRM") else Seq()
    val hideProgress =
      if (config.hideProgress) Seq(s"--$HIDE_PROGRESS") else Seq()
    val useJSON = if (config.useJSON) Seq(s"--$USE_JSON") else Seq()
    autoConfirm ++ hideProgress ++ useJSON ++ config.internalOptions.toOptions
  }
}
