package org.enso.projectmanager.infrastructure.http
import akka.actor.ActorSystem
import org.enso.jsonrpc.SecureConnectionConfig
import org.enso.projectmanager.data.Socket

/** A factory of Akka-based web socket connections.
  */
class AkkaBasedWebSocketConnectionFactory(implicit system: ActorSystem)
    extends WebSocketConnectionFactory {

  /** @inheritdoc */
  override def createConnection(socket: Socket): WebSocketConnection =
    new AkkaBasedWebSocketConnection(
      s"ws://${socket.host}:${socket.port}",
      None
    )

  /** @inheritdoc */
  override def createSecureConnection(
    socket: Socket,
    secureConfig: SecureConnectionConfig
  ): WebSocketConnection = {
    new AkkaBasedWebSocketConnection(
      s"wss://${socket.host}:${socket.port}",
      Some(secureConfig)
    )
  }
}
