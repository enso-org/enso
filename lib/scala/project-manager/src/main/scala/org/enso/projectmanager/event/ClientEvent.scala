package org.enso.projectmanager.event

import java.util.UUID

/** Base trait for all client events.
  */
sealed trait ClientEvent extends Event

object ClientEvent {

  /** Notifies the Language Server about a new client connecting.
    *
    * @param clientId an object representing a client
    * @param port the port number to which the client connected
    */
  case class ClientConnected(clientId: UUID, port: Int) extends ClientEvent

  /** Notifies the Language Server about a client disconnecting.
    * The client may not send any further messages after this one.
    *
    * @param clientId the internal id of this client
    * @param port the port number from which the client disconnected
    */
  case class ClientDisconnected(clientId: UUID, port: Int) extends ClientEvent

}
