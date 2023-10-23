package org.enso.projectmanager.infrastructure.http

import org.enso.jsonrpc.SecureConnectionConfig
import org.enso.projectmanager.data.Socket

/** Abstract connection factory.
  */
trait WebSocketConnectionFactory {

  /** Creates web socket connection.
    *
    * @param socket a server address
    * @return a connection
    */
  def createConnection(socket: Socket): WebSocketConnection

  /** Creates a secure web socket connection.
    *
    * @param socket a server address
    * @return a secure connection
    */
  def createSecureConnection(
    socket: Socket,
    secureConfig: SecureConnectionConfig
  ): WebSocketConnection

}
