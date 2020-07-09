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

  /** Run the database query in one transaction */
  def transaction[A](query: F[A]): G[A]

  /** Close the database. */
  def close(): Unit
}
