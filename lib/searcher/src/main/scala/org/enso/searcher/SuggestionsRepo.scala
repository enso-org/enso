package org.enso.searcher

/** The object for accessing the suggestions database. */
trait SuggestionsRepo[F[_]] {

  /** Get all suggestions. */
  def getAll: F[Seq[Suggestion]]

  /** Find suggestions by the return type.
    *
    * @param returnType the return type of a suggestion
    * @return the list of suggestions
    */
  def findBy(returnType: String): F[Seq[Suggestion]]

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
  def insert(suggestion: Suggestion): F[Long]
}
