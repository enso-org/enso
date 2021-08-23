package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.lockmanager.server.LockManagerService
import org.enso.polyglot.runtime.Runtime

class RuntimeRequestHandler(
  runtimeConnector: ActorRef,
  handlers: Map[Class[_], ActorRef]
) extends Actor
    with LazyLogging {
  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[Runtime.Api.Request])
  }

  override def receive: Receive = {
    case request @ Runtime.Api.Request(_, payload) =>
      handlers.get(payload.getClass) match {
        case Some(handler) =>
          handler ! request
        case None =>
          logger.warn(s"No registered handler found for request [$payload].")
      }

    case response: Runtime.Api.Response =>
      runtimeConnector ! response
  }
}

object RuntimeRequestHandler {
  def props(runtimeConnector: ActorRef, lockManagerService: ActorRef): Props = {
    val lockRequests =
      LockManagerService.handledRequestTypes.map(_ -> lockManagerService)
    val handlers: Map[Class[_], ActorRef] = Map.from(lockRequests)
    Props(new RuntimeRequestHandler(runtimeConnector, handlers))
  }
}
