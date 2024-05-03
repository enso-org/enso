package org.enso.languageserver.runtime.handler

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID

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
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import ContextRegistryProtocol._
  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case msg: Api.SetExecutionEnvironmentRequest =>
      val request = Api.Request(UUID.randomUUID(), msg)
      runtime ! request
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(sender(), request, cancellable, 10))
  }

  private def responseStage(
    replyTo: ActorRef,
    request: Api.Request,
    cancellable: Cancellable,
    retries: Int
  ): Receive = {
    case RequestTimeout =>
      if (retries > 0) {
        logger.warn(
          "Failed to receive a [{}] response in [{}]. Retrying.",
          request,
          timeout
        )
        val newCancellable =
          context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
        context.become(
          responseStage(replyTo, request, newCancellable, retries - 1)
        )
      } else {
        replyTo ! RequestTimeout
        context.stop(self)
      }

    case Api.Response(_, Api.SetExecutionEnvironmentResponse(contextId)) =>
      replyTo ! SetExecutionEnvironmentResponse(contextId)
      cancellable.cancel()
      context.stop(self)

    case Api.Response(_, error: Api.Error) =>
      runtimeFailureMapper.mapApiError(error).pipeTo(replyTo)
      cancellable.cancel()
      context.stop(self)
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
