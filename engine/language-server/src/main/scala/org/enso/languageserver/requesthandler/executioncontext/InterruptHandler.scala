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

/** A request handler for `executionContext/interrupt` commands.
  *
  * @param timeout request timeout
  * @param contextRegistry a reference to the context registry.
  * @param session an object representing a client connected to the language server
  */
class InterruptHandler(
  timeout: FiniteDuration,
  contextRegistry: ActorRef,
  session: JsonSession
) extends RequestHandlerWithRetries[
      Request[ExecutionContextInterrupt.type, ExecutionContextInterrupt.Params],
      ContextRegistryProtocol.InterruptContextResponse,
      ContextRegistryProtocol.Failure,
      ContextRegistryProtocol.InterruptContextRequest
    ](contextRegistry, timeout, 10)
    with Actor
    with LazyLogging
    with UnhandledLogging {

  override protected def request(
    msg: Request[
      ExecutionApi.ExecutionContextInterrupt.type,
      ExecutionContextInterrupt.Params
    ]
  ): ContextRegistryProtocol.InterruptContextRequest =
    ContextRegistryProtocol.InterruptContextRequest(
      session,
      msg.params.contextId
    )

  override protected def positiveResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      ExecutionApi.ExecutionContextInterrupt.type,
      ExecutionContextInterrupt.Params
    ],
    msg: ContextRegistryProtocol.InterruptContextResponse
  ): Unit =
    replyTo ! ResponseResult(ExecutionContextInterrupt, initialMsg.id, Unused)

  override protected def negativeResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      ExecutionApi.ExecutionContextInterrupt.type,
      ExecutionContextInterrupt.Params
    ],
    error: ContextRegistryProtocol.Failure
  )(implicit ec: ExecutionContext): Unit =
    replyTo ! ResponseError(
      Some(initialMsg.id),
      RuntimeFailureMapper.mapFailure(error)
    )
}

object InterruptHandler {

  /** Creates configuration object used to create an [[InterruptHandler]].
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
    Props(new InterruptHandler(timeout, contextRegistry, rpcSession))

}
