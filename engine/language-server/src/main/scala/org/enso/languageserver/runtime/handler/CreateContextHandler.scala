package org.enso.languageserver.runtime.handler

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.ExecutionProtocol
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for create context commands.
  *
  * @param timeout request timeout
  * @param runtime reference to the runtime connector
  */
final class CreateContextHandler(
  timeout: FiniteDuration,
  runtime: ActorRef
) extends Actor
    with ActorLogging {

  import context.dispatcher, ExecutionProtocol._

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case CreateContextRequest(contextId) =>
      runtime ! Api.Request(
        UUID.randomUUID(),
        Api.CreateContextRequest(contextId)
      )
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(sender(), cancellable))
  }

  private def responseStage(
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case RequestTimeout =>
      replyTo ! RequestTimeout
      context.stop(self)

    case Api.Response(_, Api.CreateContextResponse(contextId)) =>
      replyTo ! CreateContextResponse(contextId)
      cancellable.cancel()
      context.stop(self)
  }

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)
}

object CreateContextHandler {

  /**
    * Creates configuration object used to create a [[CreateContextHandler]]
    *
    * @param timeout request timeout
    * @param runtime reference to the runtime connector
    */
  def props(timeout: FiniteDuration, runtime: ActorRef): Props =
    Props(new CreateContextHandler(timeout, runtime))
}
