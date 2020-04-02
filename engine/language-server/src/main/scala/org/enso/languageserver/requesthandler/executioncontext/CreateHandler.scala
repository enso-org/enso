package org.enso.languageserver.requesthandler.executioncontext

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.languageserver.data.{
  CanModify,
  CapabilityRegistration,
  ReceivesEvents
}
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.ExecutionApi._
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  ExecutionProtocol
}

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for `executionContext/create` commands.
  *
  * @param timeout request timeout
  * @param contextRegistry a reference to the context registry.
  */
class CreateHandler(
  timeout: FiniteDuration,
  contextRegistry: ActorRef
) extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(ExecutionContextCreate, id, _) =>
      contextRegistry ! ContextRegistryProtocol.CreateContextRequest(sender())
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(id, sender(), cancellable))
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case RequestTimeout =>
      log.error(s"Request $id timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case ExecutionProtocol.CreateContextResponse(contextId) =>
      val canModify      = CapabilityRegistration(CanModify(contextId))
      val receivesEvents = CapabilityRegistration(ReceivesEvents(contextId))
      val result         = ExecutionContextCreate.Result(canModify, receivesEvents)
      replyTo ! ResponseResult(ExecutionContextCreate, id, result)
      cancellable.cancel()
      context.stop(self)
  }

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)
}

object CreateHandler {

  /**
    * Creates configuration object used to create a [[CreateHandler]].
    *
    * @param timeout request timeout
    * @param contextRegistry a reference to the context registry.
    */
  def props(timeout: FiniteDuration, contextRegistry: ActorRef): Props =
    Props(new CreateHandler(timeout, contextRegistry))

}
