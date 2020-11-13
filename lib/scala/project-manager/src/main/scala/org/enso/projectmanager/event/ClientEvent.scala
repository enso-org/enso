package org.enso.projectmanager.event

import java.util.UUID

/**
  * Base trait for all client events.
  */
sealed trait ClientEvent extends Event

object ClientEvent {

  /**
    * Notifies the Language Server about a new client connecting.
    *
    * @param clientId an object representing a client
    */
  case class ClientConnected(clientId: UUID) extends ClientEvent

  /**
    * Notifies the Language Server about a client disconnecting.
    * The client may not send any further messages after this one.
    *
    * @param clientId the internal id of this client
    */
  case class ClientDisconnected(clientId: UUID) extends ClientEvent

}
