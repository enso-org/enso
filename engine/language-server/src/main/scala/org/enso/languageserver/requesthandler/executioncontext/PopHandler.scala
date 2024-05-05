package org.enso.languageserver.requesthandler.executioncontext

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
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

/** A request handler for `executionContext/push` commands.
  *
  * @param timeout request timeout
  * @param contextRegistry a reference to the context registry.
  * @param session an object representing a client connected to the language server
  */
class PopHandler(
  timeout: FiniteDuration,
  contextRegistry: ActorRef,
  session: JsonSession
) extends RequestHandlerWithRetries[
      Request[ExecutionContextPop.type, ExecutionContextPop.Params],
      ContextRegistryProtocol.PopContextResponse,
      ContextRegistryProtocol.Failure,
      ContextRegistryProtocol.PopContextRequest
    ](contextRegistry, timeout, 10)
    with Actor
    with LazyLogging
    with UnhandledLogging {

  override protected def request(
    msg: Request[
      ExecutionApi.ExecutionContextPop.type,
      ExecutionContextPop.Params
    ]
  ): ContextRegistryProtocol.PopContextRequest =
    ContextRegistryProtocol.PopContextRequest(session, msg.params.contextId)

  override protected def positiveResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      ExecutionApi.ExecutionContextPop.type,
      ExecutionContextPop.Params
    ],
    msg: ContextRegistryProtocol.PopContextResponse
  ): Unit =
    replyTo ! ResponseResult(ExecutionContextPop, initialMsg.id, Unused)

  override protected def negativeResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      ExecutionApi.ExecutionContextPop.type,
      ExecutionContextPop.Params
    ],
    error: ContextRegistryProtocol.Failure
  )(implicit ec: ExecutionContext): Unit =
    replyTo ! ResponseError(
      Some(initialMsg.id),
      RuntimeFailureMapper.mapFailure(error)
    )
}

object PopHandler {

  /** Creates configuration object used to create a [[PopHandler]].
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
    Props(new PopHandler(timeout, contextRegistry, rpcSession))

}
