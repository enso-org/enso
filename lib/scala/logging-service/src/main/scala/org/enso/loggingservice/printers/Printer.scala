package org.enso.loggingservice.printers

import org.enso.loggingservice.internal.protocol.WSLogMessage

trait Printer {
  def print(message: WSLogMessage): Unit
  def shutdown():                   Unit
}
