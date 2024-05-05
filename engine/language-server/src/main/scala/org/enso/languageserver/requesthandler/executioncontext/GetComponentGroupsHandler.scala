package org.enso.languageserver.requesthandler.executioncontext

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.runtime.ExecutionApi._
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  ExecutionApi,
  RuntimeFailureMapper
}
import org.enso.languageserver.util.{
  RequestHandlerWithRetries,
  UnhandledLogging
}

import scala.concurrent.ExecutionContext
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
) extends RequestHandlerWithRetries[
      Request[
        ExecutionContextGetComponentGroups.type,
        ExecutionContextGetComponentGroups.Params
      ],
      ContextRegistryProtocol.GetComponentGroupsResponse,
      ContextRegistryProtocol.Failure,
      ContextRegistryProtocol.GetComponentGroupsRequest
    ](contextRegistry, timeout, 10)
    with Actor
    with LazyLogging
    with UnhandledLogging {

  override protected def request(
    msg: Request[
      ExecutionApi.ExecutionContextGetComponentGroups.type,
      ExecutionContextGetComponentGroups.Params
    ]
  ): ContextRegistryProtocol.GetComponentGroupsRequest =
    ContextRegistryProtocol.GetComponentGroupsRequest(
      clientId,
      msg.params.contextId
    )

  override protected def positiveResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      ExecutionApi.ExecutionContextGetComponentGroups.type,
      ExecutionContextGetComponentGroups.Params
    ],
    msg: ContextRegistryProtocol.GetComponentGroupsResponse
  ): Unit =
    replyTo ! ResponseResult(
      ExecutionContextGetComponentGroups,
      initialMsg.id,
      ExecutionContextGetComponentGroups.Result(msg.componentGroups)
    )

  override protected def negativeResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      ExecutionApi.ExecutionContextGetComponentGroups.type,
      ExecutionContextGetComponentGroups.Params
    ],
    error: ContextRegistryProtocol.Failure
  )(implicit ec: ExecutionContext): Unit =
    replyTo ! ResponseError(
      Some(initialMsg.id),
      RuntimeFailureMapper.mapFailure(error)
    )
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
