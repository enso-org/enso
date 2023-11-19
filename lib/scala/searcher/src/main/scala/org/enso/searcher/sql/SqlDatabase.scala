package org.enso.searcher.sql

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import org.enso.searcher.Database
import org.enso.searcher.sqlite.LockingMode
import slick.dbio.DBIO
import slick.jdbc.SQLiteProfile

import scala.concurrent.Future

/** Ths SQL database that runs Slick [[DBIO]] queries resulting in a [[Future]].
  *
  * @param config the configuration
  */
final class SqlDatabase(config: Option[Config] = None)
    extends Database[DBIO, Future] {

  private var db: SQLiteProfile.backend.Database = _

  /** @inheritdoc */
  override def run[A](query: DBIO[A]): Future[A] =
    db.run(query)

  /** @inheritdoc */
  override def open(): Unit =
    this.synchronized {
      if (db eq null) {
        db = SQLiteProfile.backend.Database
          .forConfig(
            SqlDatabase.configPath,
            config.orNull,
            classLoader = getClass.getClassLoader
          )
      }
    }

  /** @inheritdoc */
  override def close(): Unit =
    this.synchronized {
      if (db ne null) {
        db.close()
        db = null
      }
    }
}

object SqlDatabase {

  private val configPath: String =
    "searcher.db"

  /** Create in-memory [[SqlDatabase]] instance.
    *
    * @return new sql database instance
    */
  def inmem(name: String): SqlDatabase =
    fromUrl(inmemUrl(name))

  /** Create [[SqlDatabase]] instance.
    *
    * @param filename the database file path
    * @param maybeLockingMode the locking mode
    * @return new sql database instance
    */
  def apply(
    filename: File,
    maybeLockingMode: Option[LockingMode] = None
  ): SqlDatabase =
    fromUrl(jdbcUrl(filename.toString, maybeLockingMode))

  /** Create [[SqlDatabase]] instance.
    *
    * @param url the database url
    * @return new sql database instance
    */
  def fromUrl(url: String): SqlDatabase = {
    val config = ConfigFactory
      .parseString(s"""$configPath.url = "$url"""")
      .withFallback(
        ConfigFactory.load(getClass.getClassLoader)
      )
    new SqlDatabase(Some(config))
  }

  /** Create JDBC URL for in-memory database. */
  private def inmemUrl(name: String): String =
    s"jdbc:sqlite:file:$name?mode=memory&cache=shared"

  /** Create JDBC URL from the file path. */
  private def jdbcUrl(
    filename: String,
    maybeLockingMode: Option[LockingMode]
  ): String = {
    maybeLockingMode match {
      case None =>
        s"jdbc:sqlite:${escapePath(filename)}"

      case Some(lockingMode) =>
        s"jdbc:sqlite:file:${escapePath(filename)}?vfs=${lockingMode.name}"
    }
  }

  /** Escape Windows path. */
  private def escapePath(path: String): String =
    path.replace("\\", "\\\\")
}
