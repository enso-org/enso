package org.enso.loggingservice

import akka.http.scaladsl.model.Uri
import org.enso.loggingservice.printers.{Printer, StderrPrinter}

sealed trait WSLoggerMode
object WSLoggerMode {
  case class Client(endpoint: Uri) extends WSLoggerMode
  case class Server(
    port: Short,
    host: String           = "localhost",
    printers: Seq[Printer] = Seq(StderrPrinter)
  ) extends WSLoggerMode
  case class Local(printers: Seq[Printer] = Seq(StderrPrinter))
      extends WSLoggerMode
}
