package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorLogging, Props}
import akka.pattern.pipe
import org.enso.languageserver.data.Config
import org.enso.languageserver.runtime.SearchProtocol._
import org.enso.languageserver.util.UnhandledLogging
import org.enso.searcher.{SuggestionEntry, SuggestionsRepo}

import scala.concurrent.Future

/**
  * The handler of search requests.
  *
  * Handler initializes the database and responds to the search requests.
  *
  * @param config the server configuration
  * @param repo the suggestions repo
  */
final class SuggestionsHandler(config: Config, repo: SuggestionsRepo[Future])
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = {
    case GetSuggestionsDatabaseVersion =>
      repo.currentVersion
        .map(GetSuggestionsDatabaseVersionResult)
        .pipeTo(sender())

    case GetSuggestionsDatabase =>
      repo.getAll
        .map(Function.tupled(toGetSuggestionsDatabaseResult))
        .pipeTo(sender())

    case Completion(path, _, selfType, returnType, tags) =>
      val kinds = tags.map(_.map(SuggestionKind.toSuggestion))
      val file = for {
        rootFile <- config.findContentRoot(path.rootId)
      } yield path.toFile(rootFile)
      repo
        .search(None, selfType, returnType, kinds, None)
        .map(CompletionResult.tupled)
        .pipeTo(sender())
  }

  private def toGetSuggestionsDatabaseResult(
    version: Long,
    entries: Seq[SuggestionEntry]
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
    * @param config the server configuration
    * @param repo the suggestions repo
    */
  def props(config: Config, repo: SuggestionsRepo[Future]): Props =
    Props(new SuggestionsHandler(config, repo))

}
