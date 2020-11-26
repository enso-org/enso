package org.enso.languageserver.requesthandler.search

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Status}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.search.SearchApi.{
  Import,
  SuggestionsDatabaseError
}
import org.enso.languageserver.search.{SearchFailureMapper, SearchProtocol}
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `search/import` command.
  *
  * @param timeout request timeout
  * @param suggestionsHandler a reference to the suggestions handler
  */
class ImportHandler(
  timeout: FiniteDuration,
  suggestionsHandler: ActorRef
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(Import, id, Import.Params(suggestionId)) =>
      suggestionsHandler ! SearchProtocol.Import(suggestionId)
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
      log.error(ex, "Search import error")
      replyTo ! ResponseError(Some(id), SuggestionsDatabaseError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      log.error(s"Request $id timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case msg: SearchProtocol.SearchFailure =>
      replyTo ! ResponseError(Some(id), SearchFailureMapper.mapFailure(msg))

    case SearchProtocol.ImportResult(module, symbol, exports) =>
      replyTo ! ResponseResult(
        Import,
        id,
        Import.Result(module, symbol, exports)
      )
      cancellable.cancel()
      context.stop(self)
  }
}

object ImportHandler {

  /** Creates configuration object used to create a [[ImportHandler]].
    *
    * @param timeout request timeout
    * @param suggestionsHandler a reference to the suggestions handler
    */
  def props(timeout: FiniteDuration, suggestionsHandler: ActorRef): Props =
    Props(new ImportHandler(timeout, suggestionsHandler))
}
