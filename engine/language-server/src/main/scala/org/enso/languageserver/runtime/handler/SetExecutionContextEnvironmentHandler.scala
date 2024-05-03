package org.enso.languageserver.runtime.handler

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.util.{HandlerWithRetries, UnhandledLogging}
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

/** A request handler for setting execution context command.
  *
  * @param runtimeFailureMapper mapper for runtime failures
  * @param timeout request timeout
  * @param runtime reference to the runtime connector
  */
final class SetExecutionContextEnvironmentHandler(
  runtimeFailureMapper: RuntimeFailureMapper,
  timeout: FiniteDuration,
  runtime: ActorRef
) extends HandlerWithRetries[
      Api.SetExecutionEnvironmentRequest,
      Api.SetExecutionEnvironmentResponse
    ](runtime, timeout, 10)
    with Actor
    with LazyLogging
    with UnhandledLogging {
  override protected def request(
    msg: Api.SetExecutionEnvironmentRequest
  ): Api.Request =
    Api.Request(UUID.randomUUID(), msg)

  override protected def positiveResponse(
    replyTo: ActorRef,
    msg: Api.SetExecutionEnvironmentResponse
  ): Unit = {
    replyTo ! ContextRegistryProtocol.SetExecutionEnvironmentResponse(
      msg.contextId
    )
  }

  override protected def negativeResponse(replyTo: ActorRef, error: Api.Error)(
    implicit ec: ExecutionContext
  ): Unit = {
    runtimeFailureMapper.mapApiError(error).pipeTo(replyTo)
  }
}

object SetExecutionContextEnvironmentHandler {

  /** Creates configuration object used to create a [[SetExecutionContextEnvironmentHandler]].
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
    Props(
      new SetExecutionContextEnvironmentHandler(
        runtimeFailureMapper,
        timeout,
        runtime
      )
    )
}
