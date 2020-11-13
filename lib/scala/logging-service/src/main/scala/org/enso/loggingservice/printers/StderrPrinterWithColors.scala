package org.enso.loggingservice.printers
import com.typesafe.scalalogging.Logger
import org.enso.loggingservice.internal.{
  ANSIColorsMessageRenderer,
  AnsiTerminal
}
import org.enso.loggingservice.internal.protocol.WSLogMessage

import scala.io.AnsiColor

/** Prints the log messages to the standard error output with ANSI escape codes
  * that allow to display log levels in color.
  *
  * @param printExceptions specifies if attached exceptions should be printed
  */
class StderrPrinterWithColors private (printExceptions: Boolean)
    extends Printer {
  private val renderer = new ANSIColorsMessageRenderer(printExceptions)

  /** @inheritdoc
    */
  override def print(message: WSLogMessage): Unit = {
    val lines = renderer.render(message)
    System.err.println(lines)
  }

  /** @inheritdoc
    */
  override def shutdown(): Unit = {
    System.err.print(AnsiColor.RESET)
    System.err.flush()
  }
}

object StderrPrinterWithColors {

  /** Returns a color-supporting printer if color output is available in the
    * console.
    */
  def colorPrinterIfAvailable(printExceptions: Boolean): Printer =
    if (AnsiTerminal.canUseColors())
      new StderrPrinterWithColors(printExceptions)
    else new StderrPrinter(printExceptions)

  /** Returns a color-supporting printer regardless of if color output is
    * available.
    *
    * Color output may be used even if it is unavailable in cases where the
    * output is piped to another application that will support VT colors. This
    * call tries to enable the color output in the local console anyway, in case
    * it is used with the local console.
    *
    * If color support cannot be enabled and the output seems to not be piped, a
    * warning is issued that colors are not supported.
    */
  def forceCreate(printExceptions: Boolean): StderrPrinterWithColors = {
    if (!AnsiTerminal.tryEnabling() && !AnsiTerminal.isLikelyPiped) {
      Logger[StderrPrinterWithColors].warn(
        "Color output requested on stderr console, but it is unavailable. " +
        "Unless the output is handled in a special way, the log messages may " +
        "be garbled."
      )
    }
    new StderrPrinterWithColors(printExceptions)
  }
}
