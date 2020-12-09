package org.enso.languageserver.requesthandler.monitoring

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc.{Id, Request, ResponseError, ResponseResult, Unused}
import org.enso.languageserver.event.InitializedEvent
import org.enso.languageserver.monitoring.MonitoringApi

/** A request handler for `heartbeat/init` commands. */
class InitialPingHandler extends Actor with ActorLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[InitializedEvent])
  }

  override def receive: Receive = waitingForInitialization(Nil)

  private def waitingForInitialization(
    pendingRequests: List[(ActorRef, Id)]
  ): Receive = {
    case Request(MonitoringApi.InitialPing, id, Unused) =>
      context.become(
        waitingForInitialization((sender() -> id) :: pendingRequests)
      )
    case InitializedEvent.InitializationFinished =>
      for ((ref, id) <- pendingRequests) {
        ref ! ResponseResult(MonitoringApi.InitialPing, id, Unused)
      }
      context.become(initialized)
    case InitializedEvent.InitializationFailed =>
      for ((ref, id) <- pendingRequests) {
        ref ! ResponseError(Some(id), ServiceError)
      }
      context.become(failed)
    case _: InitializedEvent =>
  }

  private def initialized: Receive = {
    case Request(MonitoringApi.InitialPing, id, Unused) =>
      sender() ! ResponseResult(MonitoringApi.InitialPing, id, Unused)
  }

  private def failed: Receive = {
    case Request(MonitoringApi.InitialPing, id, Unused) =>
      ResponseError(Some(id), ServiceError)
  }
}

object InitialPingHandler {

  /** Creates a configuration object used to create a
    * [[InitialPingHandler]]
    *
    * @return a configuration object
    */
  def props: Props = Props(new InitialPingHandler)
}
