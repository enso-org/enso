package org.enso.searcher.sql

import java.io.File

import org.enso.searcher.FileVersionsRepo
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

final class SqlVersionsRepo private (db: SqlDatabase)(implicit
  ec: ExecutionContext
) extends FileVersionsRepo[Future] {

  def init: Future[Unit] =
    db.run(initQuery)

  /** @inheritdoc */
  override def getVersion(file: File): Future[Option[Array[Byte]]] =
    db.run(getVersionQuery(file))

  /** @inheritdoc */
  override def setVersion(
    file: File,
    digest: Array[Byte]
  ): Future[Option[Array[Byte]]] =
    db.run(setVersionQuery(file, digest))

  override def remove(file: File): Future[Unit] =
    db.run(removeQuery(file))

  def clean: Future[Unit] =
    db.run(cleanQuery)

  /** Close the database. */
  def close(): Unit =
    db.close()

  private def initQuery: DBIO[Unit] =
    FileDigests.schema.createIfNotExists

  private def cleanQuery: DBIO[Unit] =
    FileDigests.delete >> DBIO.successful(())

  private def getVersionQuery(file: File): DBIO[Option[Array[Byte]]] = {
    val query = for {
      row <- FileDigests
      if row.path === file.toString
    } yield row.digest
    query.result.headOption
  }

  private def setVersionQuery(
    file: File,
    bytes: Array[Byte]
  ): DBIO[Option[Array[Byte]]] = {
    val upsertQuery = FileDigests
      .insertOrUpdate(FileVersionRow(file.toString, bytes))
    for {
      version <- getVersionQuery(file)
      _       <- upsertQuery
    } yield version
  }

  private def removeQuery(file: File): DBIO[Unit] = {
    val query = for {
      row <- FileDigests
      if row.path === file.toString
    } yield row
    query.delete >> DBIO.successful(())
  }

}

object SqlVersionsRepo {

  /** Create the digests repo.
    *
    * @return the digests repo backed up by SQL database.
    */
  def apply()(implicit ec: ExecutionContext): SqlVersionsRepo = {
    new SqlVersionsRepo(new SqlDatabase())
  }

  /** Create the digests repo.
    *
    * @param path the path to the database file.
    * @return the digests repo backed up by SQL database.
    */
  def apply(path: File)(implicit ec: ExecutionContext): SqlVersionsRepo = {
    new SqlVersionsRepo(SqlDatabase(path.toString))
  }
}
