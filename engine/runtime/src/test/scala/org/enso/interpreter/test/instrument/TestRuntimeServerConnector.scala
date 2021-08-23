package org.enso.interpreter.test.instrument

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.lockmanager.server.LockManagerService
import org.enso.polyglot.runtime.Runtime.Api

class TestRuntimeServerConnector(
  lockManagerService: ActorRef,
  sendResponse: Api.Response => Unit
) extends Actor
    with LazyLogging {
  override def receive: Receive = {
    case request @ Api.Request(_, payload) =>
      if (LockManagerService.handledRequestTypes.contains(payload.getClass)) {
        lockManagerService ! request
      } else {
        logger.warn(s"Unknown request: $request")
      }

    case response: Api.Response =>
      sendResponse(response)
  }
}

object TestRuntimeServerConnector {
  def props(
    lockManagerService: ActorRef,
    sendResponse: Api.Response => Unit
  ): Props = Props(
    new TestRuntimeServerConnector(lockManagerService, sendResponse)
  )
}
