package org.enso.languageserver.requesthandler.executioncontext

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.languageserver.data.{
  CanModify,
  CapabilityRegistration,
  Client,
  ReceivesEvents
}
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.ExecutionApi._
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for `executionContext/create` commands.
  *
  * @param timeout request timeout
  * @param contextRegistry a reference to the context registry.
  * @param client an object representing a client connected to the language server
  */
class CreateHandler(
  timeout: FiniteDuration,
  contextRegistry: ActorRef,
  client: Client
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher, ContextRegistryProtocol._

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(ExecutionContextCreate, id, _) =>
      contextRegistry ! CreateContextRequest(client)
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

    case CreateContextResponse(contextId) =>
      val canModify      = CapabilityRegistration(CanModify(contextId))
      val receivesEvents = CapabilityRegistration(ReceivesEvents(contextId))
      val result         = ExecutionContextCreate.Result(canModify, receivesEvents)
      replyTo ! ResponseResult(ExecutionContextCreate, id, result)
      cancellable.cancel()
      context.stop(self)

    case error: ContextRegistryProtocol.Failure =>
      replyTo ! ResponseError(Some(id), RuntimeFailureMapper.mapFailure(error))
      cancellable.cancel()
      context.stop(self)
  }
}

object CreateHandler {

  /**
    * Creates configuration object used to create a [[CreateHandler]].
    *
    * @param timeout request timeout
    * @param contextRegistry a reference to the context registry.
    * @param client an object representing a client connected to the language server
    */
  def props(
    timeout: FiniteDuration,
    contextRegistry: ActorRef,
    client: Client
  ): Props =
    Props(new CreateHandler(timeout, contextRegistry, client))

}
