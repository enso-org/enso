package org.enso.searcher

import org.enso.polyglot.Suggestion

/** The object for accessing the suggestions database. */
trait SuggestionsRepo[F[_]] {

  /** Get current version of the repo. */
  def currentVersion: F[Long]

  /** Get all suggestions.
    *
    * @return the current database version and the list of suggestions
    */
  def getAll: F[(Long, Seq[SuggestionEntry])]

  /** Search suggestion by various parameters.
    *
    * @param module the module name search parameter
    * @param selfType the selfType search parameter
    * @param returnType the returnType search parameter
    * @param kinds the list suggestion kinds to search
    * @param position the absolute position in the text
    * @return the current database version and the list of found suggestion ids
    */
  def search(
    module: Option[String],
    selfType: Option[String],
    returnType: Option[String],
    kinds: Option[Seq[Suggestion.Kind]],
    position: Option[Suggestion.Position]
  ): F[(Long, Seq[Long])]

  /** Select the suggestion by id.
    *
    * @param id the id of a suggestion
    * @return return the suggestion
    */
  def select(id: Long): F[Option[Suggestion]]

  /** Insert the suggestion
    *
    * @param suggestion the suggestion to insert
    * @return the id of an inserted suggestion
    */
  def insert(suggestion: Suggestion): F[Option[Long]]

  /** Insert a list of suggestions
    *
    * @param suggestions the suggestions to insert
    * @return the current database version and a list of inserted suggestion ids
    */
  def insertAll(suggestions: Seq[Suggestion]): F[(Long, Seq[Option[Long]])]

  /** Remove the suggestion.
    *
    * @param suggestion the suggestion to remove
    * @return the id of removed suggestion
    */
  def remove(suggestion: Suggestion): F[Option[Long]]

  /** Remove suggestions by module name.
    *
    * @param name the module name
    * @return the current database version and a list of removed suggestion ids
    */
  def removeByModule(name: String): F[(Long, Seq[Long])]

  /** Remove a list of suggestions.
    *
    * @param suggestions the suggestions to remove
    * @return the current database version and a list of removed suggestion ids
    */
  def removeAll(suggestions: Seq[Suggestion]): F[(Long, Seq[Option[Long]])]

  /** Update a list of suggestions by external id.
    *
    * @param expressions pairs of external id and a return type
    * @return the current database version and a list of updated suggestion ids
    */
  def updateAll(
    expressions: Seq[(Suggestion.ExternalId, String)]
  ): F[(Long, Seq[Option[Long]])]
}
