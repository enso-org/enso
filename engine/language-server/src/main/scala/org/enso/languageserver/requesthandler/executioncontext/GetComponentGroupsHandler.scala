package org.enso.languageserver.requesthandler.executioncontext

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.ExecutionApi._
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `executionContext/getComponentGroups` commands.
  *
  * @param timeout request timeout
  * @param contextRegistry a reference to the context registry.
  * @param clientId a client connected to the language server
  */
class GetComponentGroupsHandler(
  timeout: FiniteDuration,
  contextRegistry: ActorRef,
  clientId: ClientId
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import ContextRegistryProtocol._
  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(
          ExecutionContextGetComponentGroups,
          id,
          params: ExecutionContextGetComponentGroups.Params
        ) =>
      contextRegistry ! GetComponentGroupsRequest(clientId, params.contextId)
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
      logger.error("Request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case GetComponentGroupsResponse(componentGroups) =>
      replyTo ! ResponseResult(
        ExecutionContextGetComponentGroups,
        id,
        ExecutionContextGetComponentGroups.Result(componentGroups)
      )
      cancellable.cancel()
      context.stop(self)

    case error: ContextRegistryProtocol.Failure =>
      replyTo ! ResponseError(Some(id), RuntimeFailureMapper.mapFailure(error))
      cancellable.cancel()
      context.stop(self)
  }
}

object GetComponentGroupsHandler {

  /** Creates configuration object used to create a
    * [[GetComponentGroupsHandler]].
    *
    * @param timeout request timeout
    * @param contextRegistry a reference to the context registry.
    * @param clientId a client connected to the language server
    */
  def props(
    timeout: FiniteDuration,
    contextRegistry: ActorRef,
    clientId: ClientId
  ): Props =
    Props(new GetComponentGroupsHandler(timeout, contextRegistry, clientId))
}
