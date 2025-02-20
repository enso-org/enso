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

/** A request handler for `executionContext/recompute` commands.
  *
  * @param timeout request timeout
  * @param contextRegistry a reference to the context registry.
  * @param session an object representing a client connected to the language server
  */
class RecomputeHandler(
  timeout: FiniteDuration,
  contextRegistry: ActorRef,
  session: JsonSession
) extends RequestHandlerWithRetries[
      Request[ExecutionContextRecompute.type, ExecutionContextRecompute.Params],
      ContextRegistryProtocol.RecomputeContextResponse,
      ContextRegistryProtocol.Failure,
      ContextRegistryProtocol.RecomputeContextRequest
    ](contextRegistry, timeout)
    with Actor
    with LazyLogging
    with UnhandledLogging {

  override protected def request(
    msg: Request[
      ExecutionApi.ExecutionContextRecompute.type,
      ExecutionContextRecompute.Params
    ]
  ): ContextRegistryProtocol.RecomputeContextRequest =
    ContextRegistryProtocol.RecomputeContextRequest(
      session,
      msg.params.contextId,
      msg.params.invalidatedExpressions,
      msg.params.executionEnvironment,
      msg.params.expressionConfigs.getOrElse(Seq())
    )

  override protected def positiveResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      ExecutionApi.ExecutionContextRecompute.type,
      ExecutionContextRecompute.Params
    ],
    msg: ContextRegistryProtocol.RecomputeContextResponse
  ): Unit =
    replyTo ! ResponseResult(ExecutionContextRecompute, initialMsg.id, Unused)

  override protected def negativeResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      ExecutionApi.ExecutionContextRecompute.type,
      ExecutionContextRecompute.Params
    ],
    error: ContextRegistryProtocol.Failure
  )(implicit ec: ExecutionContext): Unit =
    replyTo ! ResponseError(
      Some(initialMsg.id),
      RuntimeFailureMapper.mapFailure(error)
    )
}

object RecomputeHandler {

  /** Creates configuration object used to create a [[RecomputeHandler]].
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
    Props(new RecomputeHandler(timeout, contextRegistry, rpcSession))

}
