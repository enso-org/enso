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

/** A request handler for `executionContext/setExecutionEnvironment` commands.
  *
  * @param timeout request timeout
  * @param contextRegistry a reference to the context registry.
  * @param session an object representing a client connected to the language server
  */
class SetExecutionEnvironmentHandler(
  timeout: FiniteDuration,
  contextRegistry: ActorRef,
  session: JsonSession
) extends RequestHandlerWithRetries[
      Request[
        ExecutionContextRecompute.type,
        ExecutionContextSetExecutionEnvironment.Params
      ],
      ContextRegistryProtocol.SetExecutionEnvironmentResponse,
      ContextRegistryProtocol.Failure,
      ContextRegistryProtocol.SetExecutionEnvironmentRequest
    ](contextRegistry, timeout)
    with Actor
    with LazyLogging
    with UnhandledLogging {

  override protected def request(
    msg: Request[
      ExecutionApi.ExecutionContextRecompute.type,
      ExecutionContextSetExecutionEnvironment.Params
    ]
  ): ContextRegistryProtocol.SetExecutionEnvironmentRequest =
    ContextRegistryProtocol.SetExecutionEnvironmentRequest(
      session,
      msg.params.contextId,
      msg.params.executionEnvironment
    )

  override protected def positiveResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      ExecutionApi.ExecutionContextRecompute.type,
      ExecutionContextSetExecutionEnvironment.Params
    ],
    msg: ContextRegistryProtocol.SetExecutionEnvironmentResponse
  ): Unit =
    replyTo ! ResponseResult(ExecutionContextRecompute, initialMsg.id, Unused)

  override protected def negativeResponse(
    replyTo: ActorRef,
    initialMsg: Request[
      ExecutionApi.ExecutionContextRecompute.type,
      ExecutionContextSetExecutionEnvironment.Params
    ],
    error: ContextRegistryProtocol.Failure
  )(implicit ec: ExecutionContext): Unit =
    replyTo ! ResponseError(
      Some(initialMsg.id),
      RuntimeFailureMapper.mapFailure(error)
    )
}

object SetExecutionEnvironmentHandler {

  /** Creates configuration object used to create a [[SetExecutionEnvironmentHandler]].
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
    Props(
      new SetExecutionEnvironmentHandler(timeout, contextRegistry, rpcSession)
    )

}
