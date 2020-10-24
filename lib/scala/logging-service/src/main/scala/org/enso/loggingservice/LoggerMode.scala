package org.enso.loggingservice

import akka.http.scaladsl.model.Uri
import org.enso.loggingservice.printers.Printer

/** Represents modes the logging service can be running in.
  *
  * @tparam InitializationResult type that is returned when
  *                              [[LoggingServiceManager]] sets up the given
  *                              mode
  */
sealed trait LoggerMode[InitializationResult]
object LoggerMode {

  /** Forwards log messages to a logging service server.
    *
    * @param endpoint URI that is used to connect to the server via WebSockets
    */
  case class Client(endpoint: Uri) extends LoggerMode[Unit]

  /** Starts gathering messages from this and other components.
    *
    * Its initialization returns a [[ServerBinding]] that can be used to connect
    * to the initialized server.
    *
    * @param printers a list of printers that process the incoming messages
    * @param port optional port to listen at, if not provided, the OS will
    *             choose a default port; the chosen port can be extracted from
    *             the [[ServerBinding]] that is returned when the service is
    *             initialized
    * @param interface interface to listen at
    */
  case class Server(
    printers: Seq[Printer],
    port: Option[Int] = None,
    interface: String = "localhost"
  ) extends LoggerMode[ServerBinding]

  /** Processes log messages locally.
    *
    * @param printers a list of printers that process the incoming messages
    */
  case class Local(printers: Seq[Printer]) extends LoggerMode[Unit]
}
