package org.enso.languageserver.requesthandler.executioncontext

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.data.{
  CanModify,
  CapabilityRegistration,
  ReceivesUpdates
}
import org.enso.languageserver.runtime.ExecutionApi._
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  ExecutionApi,
  RuntimeFailureMapper
}
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.util.{
  RequestHandlerWithRetries,
  UnhandledLogging
}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

/** A request handler for `executionContext/create` commands.
  *
  * @param timeout request timeout
  * @param contextRegistry a reference to the context registry.
  * @param session an object representing a client connected to the language server
  */
class CreateHandler(
  timeout: FiniteDuration,
  contextRegistry: ActorRef,
  session: JsonSession
) extends RequestHandlerWithRetries[
      Request[ExecutionContextCreate.type, ExecutionContextCreate.Params],
      ContextRegistryProtocol.CreateContextResponse,
      ContextRegistryProtocol.Failure,
      ContextRegistryProtocol.CreateContextRequest
    ](contextRegistry, timeout)
    with Actor
    with LazyLogging
    with UnhandledLogging {

  override protected def request(
    msg: Request[
      ExecutionApi.ExecutionContextCreate.type,
      ExecutionContextCreate.Params
    ]
  ): ContextRegistryProtocol.CreateContextRequest =
    ContextRegistryProtocol.CreateContextRequest(session, msg.params.contextId)

  protected def positiveResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      ExecutionApi.ExecutionContextCreate.type,
      ExecutionContextCreate.Params
    ],
    msg: ContextRegistryProtocol.CreateContextResponse
  ): Unit = {
    val contextId       = msg.contextId
    val canModify       = CapabilityRegistration(CanModify(contextId))
    val receivesUpdates = CapabilityRegistration(ReceivesUpdates(contextId))
    val result =
      ExecutionContextCreate.Result(contextId, canModify, receivesUpdates)
    replyTo ! ResponseResult(ExecutionContextCreate, initialMsg.id, result)
  }

  override protected def negativeResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      ExecutionApi.ExecutionContextCreate.type,
      ExecutionContextCreate.Params
    ],
    error: ContextRegistryProtocol.Failure
  )(implicit ec: ExecutionContext): Unit =
    replyTo ! ResponseError(
      Some(initialMsg.id),
      RuntimeFailureMapper.mapFailure(error)
    )
}

object CreateHandler {

  /** Creates configuration object used to create a [[CreateHandler]].
    *
    * @param timeout request timeout
    * @param contextRegistry a reference to the context registry.
    * @param rpcSession an object representing a client connected to the language server
    */
  def props(
    timeout: FiniteDuration,
    contextRegistry: ActorRef,
    rpcSession: JsonSession
  ): Props =
    Props(new CreateHandler(timeout, contextRegistry, rpcSession))

}
