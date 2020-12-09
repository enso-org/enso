package org.enso.languageserver.requesthandler.search

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Status}
import org.enso.jsonrpc._
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.search.SearchApi.{
  InvalidateSuggestionsDatabase,
  SuggestionsDatabaseError
}
import org.enso.languageserver.search.{SearchFailureMapper, SearchProtocol}
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `search/invalidateSuggestionsDatabase` command.
  *
  * @param timeout request timeout
  * @param suggestionsHandler a reference to the suggestions handler
  */
class InvalidateSuggestionsDatabaseHandler(
  timeout: FiniteDuration,
  suggestionsHandler: ActorRef
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(InvalidateSuggestionsDatabase, id, _) =>
      suggestionsHandler ! SearchProtocol.InvalidateSuggestionsDatabase
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(id, sender(), cancellable))
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case Status.Failure(ex) =>
      log.error(ex, "InvalidateSuggestionsDatabase error")
      replyTo ! ResponseError(Some(id), SuggestionsDatabaseError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      log.error(s"Request $id timed out")
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case msg: SearchProtocol.SearchFailure =>
      replyTo ! ResponseError(Some(id), SearchFailureMapper.mapFailure(msg))

    case error: ContextRegistryProtocol.Failure =>
      replyTo ! ResponseError(Some(id), RuntimeFailureMapper.mapFailure(error))
      cancellable.cancel()
      context.stop(self)

    case SearchProtocol.InvalidateSuggestionsDatabaseResult =>
      replyTo ! ResponseResult(InvalidateSuggestionsDatabase, id, Unused)
      cancellable.cancel()
      context.stop(self)
  }
}

object InvalidateSuggestionsDatabaseHandler {

  /** Creates configuration object used to create a
    * [[InvalidateSuggestionsDatabaseHandler]].
    *
    * @param timeout request timeout
    * @param suggestionsHandler a reference to the suggestions handler
    */
  def props(
    timeout: FiniteDuration,
    suggestionsHandler: ActorRef
  ): Props =
    Props(new InvalidateSuggestionsDatabaseHandler(timeout, suggestionsHandler))

}
