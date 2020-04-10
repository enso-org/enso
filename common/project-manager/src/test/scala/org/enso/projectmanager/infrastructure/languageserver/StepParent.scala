package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.{Actor, ActorRef, Props, Terminated}
import org.enso.projectmanager.infrastructure.languageserver.StepParent.ChildTerminated

class StepParent(childProps: Props, probe: ActorRef) extends Actor {

  val child = context.actorOf(childProps)
  context.watch(child)

  override def receive: Receive = {
    case Terminated(`child`) => probe ! ChildTerminated
    case GracefulStop        => child ! GracefulStop
    case msg                 => probe.tell(msg, sender)
  }
}

object StepParent {

  case object ChildTerminated

}
