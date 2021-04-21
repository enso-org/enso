package org.enso.languageserver.runtime.handler

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import org.enso.languageserver.data.Config
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.duration.FiniteDuration

/** A request handler for create context commands.
  *
  * @param config the language server config
  * @param timeout request timeout
  * @param runtime reference to the runtime connector
  */
final class CreateContextHandler(
  config: Config,
  timeout: FiniteDuration,
  runtime: ActorRef
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher, ContextRegistryProtocol._

  override def receive: Receive = requestStage

  private def requestStage: Receive = { case msg: Api.CreateContextRequest =>
    runtime ! Api.Request(UUID.randomUUID(), msg)
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

    case Api.Response(_, error: Api.Error) =>
      replyTo ! RuntimeFailureMapper(config).mapApiError(error)
      cancellable.cancel()
      context.stop(self)
  }
}

object CreateContextHandler {

  /** Creates configuration object used to create a [[CreateContextHandler]].
    *
    * @param config the language server config
    * @param timeout request timeout
    * @param runtime reference to the runtime connector
    */
  def props(config: Config, timeout: FiniteDuration, runtime: ActorRef): Props =
    Props(new CreateContextHandler(config, timeout, runtime))
}
