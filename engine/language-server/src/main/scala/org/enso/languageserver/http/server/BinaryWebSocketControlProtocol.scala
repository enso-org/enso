package org.enso.languageserver.http.server

import akka.actor.ActorRef

/**
  * A web socket control protocol used to manage connections created by
  * a [[BinaryWebSocketServer]].
  */
object BinaryWebSocketControlProtocol {

  /**
    * Base trait for web socket events.
    */
  sealed trait WsEvent

  /**
    * Base trait for web socket commands.
    */
  sealed trait WsCommand

  /**
    * Signals that a connection has been closed.
    */
  case object ConnectionClosed extends WsEvent

  /**
    * Signals connection failure.
    *
    * @param throwable a throwable
    */
  case class ConnectionFailed(throwable: Throwable) extends WsEvent

  /**
    * Signals that a connection has been established.
    *
    * @param outboundChannel an outbound sender
    */
  case class OutboundStreamEstablished(outboundChannel: ActorRef)
      extends WsEvent

  /**
    * Command that closes a connection.
    */
  case object CloseConnection extends WsCommand

}
