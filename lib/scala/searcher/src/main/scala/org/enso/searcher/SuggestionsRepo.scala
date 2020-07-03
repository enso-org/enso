package org.enso.searcher

/** The object for accessing the suggestions database. */
trait SuggestionsRepo[F[_]] {

  /** Initialize the repo. */
  def init: F[Unit]

  /** Clean the repo. */
  def clean: F[Unit]

  /** Get current version of the repo. */
  def currentVersion: F[Long]

  /** Get all suggestions.
    *
    * @return the current database version and the list of suggestions
    */
  def getAll: F[(Long, Seq[SuggestionEntry])]

  /** Search suggestion by various parameters.
    *
    * @param selfType the selfType search parameter
    * @param returnType the returnType search parameter
    * @param kinds the list suggestion kinds to search
    * @return the current database version and the list of found suggestion ids
    */
  def search(
    selfType: Option[String],
    returnType: Option[String],
    kinds: Option[Seq[Suggestion.Kind]]
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

  /** Remove a list of suggestions.
    *
    * @param suggestions the suggestions to remove
    * @return the current database version and a list of removed suggestion ids
    */
  def removeAll(suggestions: Seq[Suggestion]): F[(Long, Seq[Option[Long]])]
}
