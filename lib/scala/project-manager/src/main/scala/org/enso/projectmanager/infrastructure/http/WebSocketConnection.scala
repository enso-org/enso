package org.enso.projectmanager.infrastructure.http

import akka.actor.ActorRef

/** An abstraction representing web socket connection. */
trait WebSocketConnection {

  /** Connects to the server. */
  def connect(): Unit

  /** Disconnects from the server. */
  def disconnect(): Unit

  /** Close the connection and clean up resources. */
  def close(): Unit

  /** Sends a message to the server.
    *
    * @param message a message to sent
    */
  def send(message: String): Unit

  /** Attaches a listener of incoming messages.
    *
    * @param listener a message listener for inbound channel
    */
  def attachListener(listener: ActorRef): Unit

  /** Removes the listener of incoming messages.
    *
    * Can be useful when disconnecting has timed out and we do not want to
    * receive the disconnected message after the owning actor is long dead.
    */
  def detachListener(listener: ActorRef): Unit
}

object WebSocketConnection {

  /** Signals that a connection was established. */
  case object WebSocketConnected

  /** An envelope for text messages.
    *
    * @param payload a text message
    */
  case class WebSocketMessage(payload: String)

  /** Signals that connection was closed. */
  case object WebSocketStreamClosed

  /** Signals a connection failure.
    *
    * @param th a throwable
    */
  case class WebSocketStreamFailure(th: Throwable)

}
