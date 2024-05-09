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

/** A request handler for push commands.
  *
  * @param runtimeFailureMapper mapper for runtime failures
  * @param timeout request timeout
  * @param runtime reference to the runtime conector
  */
final class PopContextHandler(
  runtimeFailureMapper: RuntimeFailureMapper,
  timeout: FiniteDuration,
  runtime: ActorRef
) extends ApiHandlerWithRetries[
      Api.PopContextRequest,
      Api.PopContextResponse
    ](runtime, timeout)
    with Actor
    with LazyLogging
    with UnhandledLogging {

  override protected def request(msg: Api.PopContextRequest): Api.Request =
    Api.Request(UUID.randomUUID(), msg)

  override protected def positiveResponse(
    replyTo: ActorRef,
    msg: Api.PopContextResponse
  ): Unit =
    replyTo ! ContextRegistryProtocol.PopContextResponse(msg.contextId)

  override protected def negativeResponse(replyTo: ActorRef, error: Api.Error)(
    implicit ec: ExecutionContext
  ): Unit =
    runtimeFailureMapper.mapApiError(error).pipeTo(replyTo)
}

object PopContextHandler {

  /** Creates a configuration object used to create [[PopContextHandler]].
    *
    * @param runtimeFailureMapper mapper for runtime failures
    * @param timeout request timeout
    * @param runtime reference to the runtime conector
    */
  def props(
    runtimeFailureMapper: RuntimeFailureMapper,
    timeout: FiniteDuration,
    runtime: ActorRef
  ): Props =
    Props(new PopContextHandler(runtimeFailureMapper, timeout, runtime))
}
