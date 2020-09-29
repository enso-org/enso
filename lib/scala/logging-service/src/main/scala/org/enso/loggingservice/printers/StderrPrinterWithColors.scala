package org.enso.loggingservice.printers
import org.enso.loggingservice.internal.{
  ANSIColorsMessageRenderer,
  AnsiTerminal
}
import org.enso.loggingservice.internal.protocol.WSLogMessage

import scala.io.AnsiColor

object StderrPrinterWithColors extends Printer {
  private val renderer = new ANSIColorsMessageRenderer

  override def print(message: WSLogMessage): Unit = {
    val lines = renderer.render(message)
    System.err.println(lines)
  }

  override def shutdown(): Unit = {
    System.err.println(AnsiColor.RESET)
    System.err.flush()
  }

  def canUseColors: Boolean = {
    (System.console() != null) && AnsiTerminal.tryEnabling()
  }

  def colorPrinterIfAvailable(): Printer =
    if (canUseColors) this else StderrPrinter
}
