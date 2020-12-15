package org.enso.languageserver.monitoring

object MonitoringProtocol {

  /** A ping command.
    */
  case object Ping

  /** A pong reply.
    */
  case object Pong

  case object IsHealthy

  sealed trait HealthinessResponse

  case object KO extends HealthinessResponse

  case object OK extends HealthinessResponse

}
