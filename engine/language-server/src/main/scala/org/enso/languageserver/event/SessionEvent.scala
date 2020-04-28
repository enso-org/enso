package org.enso.languageserver.event

import org.enso.languageserver.session.{DataSession, RpcSession}

/**
  * Base trait for all session events.
  */
sealed trait SessionEvent extends Event

/**
  * Notifies the Language Server about a new rpc session.
  *
  * @param session an object representing a client session
  */
case class RpcSessionInitialized(session: RpcSession) extends SessionEvent

/**
  * Notifies the Language Server about a client disconnecting rpc session.
  * The client may not send any further messages after this one.
  *
  * @param session an object representing a client session
  */
case class RpcSessionTerminated(session: RpcSession) extends SessionEvent

/**
  * Notifies the Language Server about a new data session.
  *
  * @param session an object representing a client session
  */
case class DataSessionInitialized(session: DataSession) extends SessionEvent

/**
  * Notifies the Language Server about a client disconnecting data session.
  * The client may not send any further messages after this one.
  *
  * @param session an object representing a client session
  */
case class DataSessionTerminated(session: DataSession) extends SessionEvent
