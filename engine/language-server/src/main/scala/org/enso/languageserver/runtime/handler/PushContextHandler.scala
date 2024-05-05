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
final class PushContextHandler(
  runtimeFailureMapper: RuntimeFailureMapper,
  timeout: FiniteDuration,
  runtime: ActorRef
) extends ApiHandlerWithRetries[
      Api.PushContextRequest,
      Api.PushContextResponse
    ](
      runtime,
      timeout,
      5
    )
    with Actor
    with LazyLogging
    with UnhandledLogging {

  override protected def request(msg: Api.PushContextRequest): Api.Request =
    Api.Request(UUID.randomUUID(), msg)

  override protected def positiveResponse(
    replyTo: ActorRef,
    msg: Api.PushContextResponse
  ): Unit =
    replyTo ! ContextRegistryProtocol.PushContextResponse(msg.contextId)

  override protected def negativeResponse(replyTo: ActorRef, error: Api.Error)(
    implicit ec: ExecutionContext
  ): Unit =
    runtimeFailureMapper.mapApiError(error).pipeTo(replyTo)
}

object PushContextHandler {

  /** Creates a configuration object used to create [[PushContextHandler]].
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
    Props(new PushContextHandler(runtimeFailureMapper, timeout, runtime))
}
