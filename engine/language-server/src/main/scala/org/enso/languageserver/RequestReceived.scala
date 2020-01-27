package org.enso.languageserver

import akka.actor.ActorRef

/** Language server responses to [[Requests]]. */
object RequestReceived {

  /** Language server response to [[Requests.Initialize]]. */
  case class Initialize(id: Id, replyTo: ActorRef)

  /** Language server response to [[Requests.Shutdown]]. */
  case class Shutdown(id: Id, replyTo: ActorRef)

}
