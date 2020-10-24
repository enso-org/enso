package org.enso.projectmanager.infrastructure.http

import akka.actor.{Actor, ActorRef}
import org.enso.projectmanager.infrastructure.http.FanOutReceiver.Listen

/** A fan-out receiver that delivers messages to multiple listeners.
  */
class FanOutReceiver extends Actor {

  override def receive: Receive = running()

  private def running(listeners: Set[ActorRef] = Set.empty): Receive = {
    case Listen(listener) => context.become(running(listeners + listener))
    case msg              => listeners.foreach(_ ! msg)
  }

}

object FanOutReceiver {

  /** An attach listener command.
    *
    * @param listener a listener to attach
    */
  case class Listen(listener: ActorRef)

}
