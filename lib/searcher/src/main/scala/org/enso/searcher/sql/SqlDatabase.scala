package org.enso.searcher.sql

import org.enso.searcher.Database
import slick.dbio.DBIO
import slick.jdbc.SQLiteProfile

import scala.concurrent.Future

class SqlDatabase(path: String = "searcher.db") extends Database[DBIO, Future] {

  private val db = SQLiteProfile.api.Database.forConfig(path)

  override def run[A](program: DBIO[A]): Future[A] =
    db.run(program)

  override def close(): Unit =
    db.close()
}
