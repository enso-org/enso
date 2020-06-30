package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorLogging, Props}
import akka.pattern.pipe
import org.enso.languageserver.runtime.SearchProtocol.{
  Complete,
  CompleteResult,
  GetSuggestionsDatabase,
  GetSuggestionsDatabaseResult,
  GetSuggestionsDatabaseVersion,
  GetSuggestionsDatabaseVersionResult,
  SuggestionKind,
  SuggestionsDatabaseUpdate
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.searcher.SuggestionEntry
import org.enso.searcher.sql.{SqlDatabase, SqlSuggestionsRepo}

/**
  * Event listener listens event stream for the suggestion database
  * notifications from the runtime and sends updates to the client. The listener
  * is a singleton and created per context registry.
  */
final class SuggestionsHandler(
  repo: SqlSuggestionsRepo,
  db: SqlDatabase
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    db.run(repo.init)
  }

  override def postStop(): Unit = {
    db.close()
  }

  override def receive: Receive = {
    case GetSuggestionsDatabaseVersion =>
      db
        .run(repo.currentVersion)
        .map(GetSuggestionsDatabaseVersionResult)
        .pipeTo(sender())

    case GetSuggestionsDatabase =>
      val query = for {
        entries <- repo.getAll
        version <- repo.currentVersion
      } yield (entries, version)
      db
        .run(query)
        .map(Function.tupled(toGetSuggestionsDatabaseResult))
        .pipeTo(sender())

    case Complete(_, _, selfType, returnType, tags) =>
      val kinds = tags.map(_.map(SuggestionKind.toSuggestion))
      val query = for {
        entries <- repo.search(selfType, returnType, kinds)
        version <- repo.currentVersion
      } yield (entries, version)
      db
        .run(query)
        .map(CompleteResult.tupled)
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
    */
  def props(
    repo: SqlSuggestionsRepo,
    db: SqlDatabase
  ): Props =
    Props(new SuggestionsHandler(repo, db))

}
