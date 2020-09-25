package org.enso.loggingservice

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.{Authority, Host, Path}
import org.enso.loggingservice.printers.{Printer, StderrPrinter}

sealed trait WSLoggerMode[InitializationResult]
object WSLoggerMode {

  /**
    * Forwards log messages to a logging service server.
    *
    * @param endpoint URI that is used to connect to the server via WebSockets
    */
  case class Client(endpoint: Uri) extends WSLoggerMode[Unit]

  /**
    * Starts gathering messages from this and other components.
    *
    * @param printers a list of printers that process the incoming messages
    * @param port optional port to listen at, if not provided, the OS will
    *             choose a default port; the chosen port can be extracted from
    *             the [[ServerBinding]] that is returned when the service is
    *             initialized
    * @param interface interface to listen at
    */
  case class Server(
    printers: Seq[Printer] = Seq(StderrPrinter),
    port: Option[Int]      = None,
    interface: String      = "localhost"
  ) extends WSLoggerMode[ServerBinding]

  /**
    * Processes log messages locally.
    *
    * @param printers a list of printers that process the incoming messages
    */
  case class Local(printers: Seq[Printer] = Seq(StderrPrinter))
      extends WSLoggerMode[Unit]

  case class ServerBinding(port: Int) {
    def toUri(host: String = "localhost"): Uri =
      Uri(
        scheme    = "ws",
        authority = Authority(host = Host(host), port = port),
        path      = Path./
      )
  }
}
