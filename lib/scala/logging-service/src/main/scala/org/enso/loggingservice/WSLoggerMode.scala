package org.enso.loggingservice

sealed trait WSLoggerMode
object WSLoggerMode {
  case class Client(ip: String, port: Short) extends WSLoggerMode
  case class Server(
    port: Short,
    host: String = "localhost",
    config: LoggingConfig
  ) extends WSLoggerMode
  case class Local(config: LoggingConfig = LoggingConfig.Default)
      extends WSLoggerMode
}
