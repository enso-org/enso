package org.enso.loggingservice.internal

object StderrPrinter {
  def print(logMessage: WSLogMessage): Unit = {
    val lines = DefaultLogMessageRenderer.render(logMessage)
    System.err.println(lines)
  }

  def printColors(logMessage: WSLogMessage): Unit = ???
}
