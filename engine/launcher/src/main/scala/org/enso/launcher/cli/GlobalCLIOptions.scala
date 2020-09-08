package org.enso.launcher.cli

/**
  * Gathers settings set by the global CLI options.
  *
  * @param autoConfirm if this flag is set, the program should not ask the user
  *                    any questions but proceed with the default values, that
  *                    must be explained in the help text for each command
  * @param hideProgress if this flag is set, progress bars should not be
  *                     printed
  * @param useJSON specifies if output should be in JSON format, if it is
  *                supported (currently only the version command supports JSON)
  */
case class GlobalCLIOptions(
  autoConfirm: Boolean,
  hideProgress: Boolean,
  useJSON: Boolean
)

object GlobalCLIOptions {
  val HIDE_PROGRESS = "hide-progress"
  val AUTO_CONFIRM  = "auto-confirm"
  val USE_JSON      = "json"

  /**
    * Converts the [[GlobalCLIOptions]] to a sequence of arguments that can be
    * added to a launcher invocation to set the same options.
    */
  def toOptions(config: GlobalCLIOptions): Seq[String] = {
    val autoConfirm = if (config.autoConfirm) Seq(s"--$AUTO_CONFIRM") else Seq()
    val hideProgress =
      if (config.hideProgress) Seq(s"--$HIDE_PROGRESS") else Seq()
    val useJSON = if (config.useJSON) Seq(s"--$USE_JSON") else Seq()
    autoConfirm ++ hideProgress ++ useJSON
  }
}
