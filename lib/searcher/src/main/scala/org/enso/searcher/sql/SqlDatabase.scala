package org.enso.searcher.sql

import org.enso.searcher.Database
import slick.dbio.DBIO
import slick.jdbc.SQLiteProfile

import scala.concurrent.Future

/** Ths SQL database that runs Slick [[DBIO]] queries resulting in a [[Future]].
  *
  * @param path the configuration path
  */
class SqlDatabase(path: String = "searcher.db") extends Database[DBIO, Future] {

  private val db = SQLiteProfile.api.Database.forConfig(path)

  /** @inheritdoc */
  override def run[A](query: DBIO[A]): Future[A] =
    db.run(query)

  /** @inheritdoc */
  override def close(): Unit =
    db.close()
}
