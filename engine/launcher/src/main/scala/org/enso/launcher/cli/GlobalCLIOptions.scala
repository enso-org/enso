package org.enso.launcher.cli

import akka.http.scaladsl.model.Uri
import org.enso.cli.arguments.{Argument, OptsParseError}
import org.enso.launcher.cli.GlobalCLIOptions.InternalOptions
import org.enso.loggingservice.LogLevel

/** Gathers settings set by the global CLI options.
  *
  * @param autoConfirm if this flag is set, the program should not ask the user
  *                    any questions but proceed with the default values, that
  *                    must be explained in the help text for each command
  * @param hideProgress if this flag is set, progress bars should not be
  *                     printed
  * @param useJSON specifies if output should be in JSON format, if it is
  *                supported (currently only the version command supports JSON)
  * @param colorMode specifies if console output should contain colors
  * @param internalOptions options that are remembered to pass them to launcher
  *                        child processes
  */
case class GlobalCLIOptions(
  autoConfirm: Boolean,
  hideProgress: Boolean,
  useJSON: Boolean,
  colorMode: ColorMode,
  internalOptions: InternalOptions = InternalOptions(None, None)
)

object GlobalCLIOptions {
  val HIDE_PROGRESS = "hide-progress"
  val AUTO_CONFIRM  = "auto-confirm"
  val USE_JSON      = "json"
  val COLOR_MODE    = "color"

  /** Internal options that are remembered to pass them to launcher child
    * processes.
    */
  case class InternalOptions(
    launcherLogLevel: Option[LogLevel],
    loggerConnectUri: Option[Uri]
  ) {

    /** Creates command line options that can be passed to a launcher process to
      * set the same options.
      */
    def toOptions: Seq[String] = {
      val level = launcherLogLevel
        .map(level => Seq(s"--$LOG_LEVEL", level.toString))
        .getOrElse(Seq())
      val uri = loggerConnectUri
        .map(uri => Seq(s"--$CONNECT_LOGGER", uri.toString))
        .getOrElse(Seq())
      level ++ uri
    }
  }

  val LOG_LEVEL      = "launcher-log-level"
  val CONNECT_LOGGER = "internal-connect-logger"

  /** Converts the [[GlobalCLIOptions]] to a sequence of arguments that can be
    * added to a launcher invocation to set the same options.
    */
  def toOptions(config: GlobalCLIOptions): Seq[String] = {
    val autoConfirm = if (config.autoConfirm) Seq(s"--$AUTO_CONFIRM") else Seq()
    val hideProgress =
      if (config.hideProgress) Seq(s"--$HIDE_PROGRESS") else Seq()
    val useJSON = if (config.useJSON) Seq(s"--$USE_JSON") else Seq()
    autoConfirm ++ hideProgress ++ useJSON ++
    ColorMode.toOptions(config.colorMode) ++ config.internalOptions.toOptions
  }
}

/** Describes possible modes of color display in console output.
  */
sealed trait ColorMode
object ColorMode {

  /** Never use color escape sequences in the output.
    */
  case object Never extends ColorMode

  /** Enable color output if it seems to be supported.
    */
  case object Auto extends ColorMode

  /** Always use escape sequences in the output, even if the program thinks they
    * are unsupported.
    *
    * May be useful if output is piped to other programs that know how to handle
    * the escape sequences.
    */
  case object Always extends ColorMode

  /** [[Argument]] instance used to parse [[ColorMode]] from CLI.
    */
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

  /** Creates command line options that can be passed to a launcher process to
    * inherit our color mode.
    */
  def toOptions(colorMode: ColorMode): Seq[String] = {
    val name = colorMode match {
      case Never  => "never"
      case Auto   => "auto"
      case Always => "always"
    }
    Seq(s"--${GlobalCLIOptions.COLOR_MODE}", name)
  }
}
