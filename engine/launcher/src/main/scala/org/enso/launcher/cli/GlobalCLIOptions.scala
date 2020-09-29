package org.enso.launcher.cli

import org.enso.cli.arguments.{Argument, OptsParseError}

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
  * @param colorMode specifies if console output should contain colors
  */
case class GlobalCLIOptions(
  autoConfirm: Boolean,
  hideProgress: Boolean,
  useJSON: Boolean,
  colorMode: ColorMode
)

object GlobalCLIOptions {
  val HIDE_PROGRESS = "hide-progress"
  val AUTO_CONFIRM  = "auto-confirm"
  val USE_JSON      = "json"
  val COLOR_MODE    = "color"

  // TODO ensure these are inherited by child launchers etc.
  val LOG_LEVEL      = "launcher-log-level"
  val CONNECT_LOGGER = "internal-connect-logger"

  /**
    * Converts the [[GlobalCLIOptions]] to a sequence of arguments that can be
    * added to a launcher invocation to set the same options.
    */
  def toOptions(config: GlobalCLIOptions): Seq[String] = {
    val autoConfirm = if (config.autoConfirm) Seq(s"--$AUTO_CONFIRM") else Seq()
    val hideProgress =
      if (config.hideProgress) Seq(s"--$HIDE_PROGRESS") else Seq()
    val useJSON = if (config.useJSON) Seq(s"--$USE_JSON") else Seq()
    autoConfirm ++ hideProgress ++ useJSON ++
    ColorMode.toOptions(config.colorMode)
  }
}

sealed trait ColorMode
object ColorMode {
  case object Never  extends ColorMode
  case object Auto   extends ColorMode
  case object Always extends ColorMode

  implicit val argument: Argument[ColorMode] = {
    case "never"  => Right(Never)
    case "no"     => Right(Never)
    case "auto"   => Right(Auto)
    case "always" => Right(Always)
    case "yes"    => Right(Always)
    case other =>
      OptsParseError.left(
        s"Unknown color mode value `$other`. Supported values are: " +
        s"never | no | auto | always | yes."
      )
  }

  def toOptions(colorMode: ColorMode): Seq[String] = {
    val name = colorMode match {
      case Never  => "never"
      case Auto   => "auto"
      case Always => "always"
    }
    Seq(s"--${GlobalCLIOptions.COLOR_MODE}", name)
  }
}
