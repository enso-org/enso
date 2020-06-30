package org.enso.searcher.sql

import org.enso.searcher.Database
import slick.dbio.DBIO
import slick.jdbc.SQLiteProfile

import scala.concurrent.Future

/** Ths SQL database that runs Slick [[DBIO]] queries resulting in a [[Future]].
  *
  * @param filename the database filename
  */
class SqlDatabase(filename: String = ":memory:?cache=shared")
    extends Database[DBIO, Future] {

  private val db = SQLiteProfile.api.Database.forURL(
    url                 = s"jdbc:sqlite:file:$filename",
    driver              = "org.sqlite.JDBC",
    keepAliveConnection = true
  )

  /** @inheritdoc */
  override def run[A](query: DBIO[A]): Future[A] =
    db.run(query)

  /** @inheritdoc */
  override def close(): Unit =
    db.close()
}
