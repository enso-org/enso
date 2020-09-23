package org.enso.loggingservice.server

import org.enso.loggingservice.internal.{
  InternalLogMessage,
  Level,
  LoggerConnection
}

object WSLoggerServer {

  object Connection extends LoggerConnection {
    override def send(message: InternalLogMessage): Unit = ???
    override def logLevel: Level                         = ???
  }
  def start(): Unit = {}
}
