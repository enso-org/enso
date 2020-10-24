package org.enso.languageserver.search.handler

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.RuntimeFailureMapper
import org.enso.languageserver.search.SearchProtocol
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.duration.FiniteDuration

/** A request handler for invalidate modules index command.
  *
  * @param timeout request timeout
  * @param runtime reference to the runtime connector
  */
final class InvalidateModulesIndexHandler(
  timeout: FiniteDuration,
  runtime: ActorRef
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case SearchProtocol.InvalidateModulesIndex =>
      runtime ! Api.Request(
        UUID.randomUUID(),
        Api.InvalidateModulesIndexRequest()
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

    case Api.Response(_, Api.InvalidateModulesIndexResponse()) =>
      replyTo ! SearchProtocol.InvalidateSuggestionsDatabaseResult
      cancellable.cancel()
      context.stop(self)

    case Api.Response(_, error: Api.Error) =>
      replyTo ! RuntimeFailureMapper.mapApiError(error)
      cancellable.cancel()
      context.stop(self)
  }
}

object InvalidateModulesIndexHandler {

  /** Creates a configuration object used to create [[InvalidateModulesIndexHandler]].
    *
    * @param timeout request timeout
    * @param runtime reference to the runtime conector
    */
  def props(timeout: FiniteDuration, runtime: ActorRef): Props =
    Props(new InvalidateModulesIndexHandler(timeout, runtime))
}
