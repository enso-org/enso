package org.enso.searcher

/** The database queries
  *
  * @tparam F the type of the query
  * @tparam G the type of the result
  */
trait Database[F[_], G[_]] {

  /** Run the database query.
    *
    * @param query the query to run
    */
  def run[A](query: F[A]): G[A]

  /** Open the database. */
  def open(): Unit

  /** Close the database. */
  def close(): Unit
}
