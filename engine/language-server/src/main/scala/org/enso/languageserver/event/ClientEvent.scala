package org.enso.languageserver.event

import org.enso.languageserver.data.Client

/**
  * Base trait for all client events.
  */
sealed trait ClientEvent extends Event

/**
  * Notifies the Language Server about a new client connecting.
  *
  * @param client an object representing a client
  */
case class ClientConnected(client: Client) extends ClientEvent

/**
  * Notifies the Language Server about a client disconnecting.
  * The client may not send any further messages after this one.
  *
  * @param client an object representing a client
  */
case class ClientDisconnected(client: Client) extends ClientEvent
