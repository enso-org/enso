package org.enso.languageserver.runtime

import org.enso.searcher.Suggestion

object SuggestionsDatabaseEventsApi {

  sealed trait SuggestionsDatabaseUpdate
  object SuggestionsDatabaseUpdate {

    /** Create or replace the database entry.
      *
      * @param id suggestion id
      * @param suggestion the new suggestion
      */
    case class Add(id: Long, suggestion: Suggestion)
        extends SuggestionsDatabaseUpdate

    /** Remove the database entry.
      *
      * @param id the suggestion id
      */
    case class Remove(id: Long) extends SuggestionsDatabaseUpdate

    /** Modify the database entry.
      *
      * @param id the suggestion id
      * @param name the new suggestion name
      * @param arguments the new suggestion arguments
      * @param selfType the new self type of the suggestion
      * @param returnType the new return type of the suggestion
      * @param documentation the new documentation string
      * @param scope the new scope
      */
    case class Modify(
      id: Long,
      name: Option[String],
      arguments: Option[Seq[Suggestion.Argument]],
      selfType: Option[String],
      returnType: Option[String],
      documentation: Option[String],
      scope: Option[Suggestion.Scope]
    ) extends SuggestionsDatabaseUpdate

  }
}
