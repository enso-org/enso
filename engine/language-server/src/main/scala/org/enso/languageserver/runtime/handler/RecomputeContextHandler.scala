package org.enso.languageserver.runtime.handler

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.util.{ApiHandlerWithRetries, UnhandledLogging}
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

/** A request handler for recompute commands.
  *
  * @param runtimeFailureMapper mapper for runtime failures
  * @param timeout request timeout
  * @param runtime reference to the runtime connector
  */
final class RecomputeContextHandler(
  runtimeFailureMapper: RuntimeFailureMapper,
  timeout: FiniteDuration,
  runtime: ActorRef
) extends ApiHandlerWithRetries[
      Api.RecomputeContextRequest,
      Api.RecomputeContextResponse
    ](runtime, timeout)
    with Actor
    with LazyLogging
    with UnhandledLogging {

  override protected def request(
    msg: Api.RecomputeContextRequest
  ): Api.Request =
    Api.Request(UUID.randomUUID(), msg)

  override protected def positiveResponse(
    replyTo: ActorRef,
    msg: Api.RecomputeContextResponse
  ): Unit =
    replyTo ! ContextRegistryProtocol.RecomputeContextResponse(msg.contextId)

  override protected def negativeResponse(replyTo: ActorRef, error: Api.Error)(
    implicit ec: ExecutionContext
  ): Unit =
    runtimeFailureMapper.mapApiError(error).pipeTo(replyTo)
}

object RecomputeContextHandler {

  /** Creates configuration object used to create a [[RecomputeContextHandler]].
    *
    * @param runtimeFailureMapper mapper for runtime failures
    * @param timeout request timeout
    * @param runtime reference to the runtime connector
    */
  def props(
    runtimeFailureMapper: RuntimeFailureMapper,
    timeout: FiniteDuration,
    runtime: ActorRef
  ): Props =
    Props(new RecomputeContextHandler(runtimeFailureMapper, timeout, runtime))
}
