package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorLogging, Props}
import akka.pattern.pipe
import org.enso.languageserver.runtime.SearchProtocol.{
  Completion,
  CompletionResult,
  GetSuggestionsDatabase,
  GetSuggestionsDatabaseResult,
  GetSuggestionsDatabaseVersion,
  GetSuggestionsDatabaseVersionResult,
  SuggestionKind,
  SuggestionsDatabaseUpdate
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.searcher.{SuggestionEntry, SuggestionsRepo}

import scala.concurrent.Future

/**
  * The handler of search requests.
  *
  * Handler initializes the database and responds to the search requests.
  *
  * @param repo the suggestions repo
  */
final class SuggestionsHandler(
  repo: SuggestionsRepo[Future]
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    repo.init
  }

  override def receive: Receive = {
    case GetSuggestionsDatabaseVersion =>
      repo.currentVersion
        .map(GetSuggestionsDatabaseVersionResult)
        .pipeTo(sender())

    case GetSuggestionsDatabase =>
      val query = for {
        entries <- repo.getAll
        version <- repo.currentVersion
      } yield (entries, version)
      query
        .map(Function.tupled(toGetSuggestionsDatabaseResult))
        .pipeTo(sender())

    case Completion(_, _, selfType, returnType, tags) =>
      val kinds = tags.map(_.map(SuggestionKind.toSuggestion))
      val query = for {
        entries <- repo.search(selfType, returnType, kinds)
        version <- repo.currentVersion
      } yield (entries, version)
      query
        .map(CompletionResult.tupled)
        .pipeTo(sender())
  }

  private def toGetSuggestionsDatabaseResult(
    entries: Seq[SuggestionEntry],
    version: Long
  ): GetSuggestionsDatabaseResult = {
    val updates = entries.map(entry =>
      SuggestionsDatabaseUpdate.Add(entry.id, entry.suggestion)
    )
    GetSuggestionsDatabaseResult(updates, version)
  }
}

object SuggestionsHandler {

  /**
    * Creates a configuration object used to create a [[SuggestionsHandler]].
    *
    * @param repo the suggestions repo
    */
  def props(repo: SuggestionsRepo[Future]): Props =
    Props(new SuggestionsHandler(repo))

}
