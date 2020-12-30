package org.enso.languageserver.monitoring

object MonitoringProtocol {

  /**
    * A ping command.
    */
  case object Ping

  /**
    * A pong reply.
    */
  case object Pong

}
