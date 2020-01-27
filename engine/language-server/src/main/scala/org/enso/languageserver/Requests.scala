package org.enso.languageserver

import akka.actor.ActorRef

/** Akka messages sent by Gateway received LSP requests. */
object Requests {

  /** Akka message sent by Gateway received LSP request `initialize`. */
  case class Initialize(
    id: Id,
    replyTo: ActorRef
  )

  /** Akka message sent by Gateway received LSP request `shutdown`. */
  case class Shutdown(id: Id, replyTo: ActorRef)

}
