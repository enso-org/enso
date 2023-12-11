package org.enso.interpreter.test.instrument

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.lockmanager.server.LockManagerService
import org.enso.polyglot.runtime.Runtime.Api

/** A helper Actor that is used to pass messages from the runtime to the lock
  * manager service and vice-versa.
  *
  * @param lockManagerService reference to the lock manager service actor
  * @param sendResponse a callback used to send the responses from the lock
  *                     manager service back to the runtime
  */
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
