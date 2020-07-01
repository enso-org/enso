package org.enso.searcher.sql

import com.typesafe.config.{Config, ConfigFactory}
import org.enso.searcher.Database
import slick.dbio.DBIO
import slick.jdbc.SQLiteProfile
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.Future

/** Ths SQL database that runs Slick [[DBIO]] queries resulting in a [[Future]].
  *
  * @param config the configuration
  */
final class SqlDatabase(config: Option[Config] = None)
    extends Database[DBIO, Future] {

  val db = SQLiteProfile.backend.Database
    .forConfig(SqlDatabase.configPath, config.orNull)

  /** @inheritdoc */
  override def run[A](query: DBIO[A]): Future[A] =
    db.run(query)

  /** @inheritdoc */
  override def transaction[A](query: DBIO[A]): Future[A] =
    db.run(query.transactionally)

  /** @inheritdoc */
  override def close(): Unit =
    db.close()
}

object SqlDatabase {

  val configPath: String =
    "searcher.db"

  def apply(filename: String): SqlDatabase = {
    val config = ConfigFactory
      .parseString(s"$configPath.filename = $filename")
      .withFallback(ConfigFactory.load())
    new SqlDatabase(Some(config))
  }
}
