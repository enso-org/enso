package org.enso.searcher.sql

import java.io.File
import java.util

import org.enso.searcher.FileVersionsRepo
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

final class SqlVersionsRepo(db: SqlDatabase)(implicit ec: ExecutionContext)
    extends FileVersionsRepo[Future] {

  /** Initialize the repo. */
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

  /** @inheritdoc */
  override def updateVersion(file: File, digest: Array[Byte]): Future[Boolean] =
    db.run(updateVersionQuery(file, digest))

  /** @inheritdoc */
  override def updateVersions(
    versions: Seq[(File, Array[Byte])]
  ): Future[Unit] =
    db.run(updateVersionsQuery(versions))

  /** @inheritdoc */
  override def remove(file: File): Future[Unit] =
    db.run(removeQuery(file))

  /** @inheritdoc */
  override def clean: Future[Unit] =
    db.run(cleanQuery)

  /** Close the database. */
  def close(): Unit =
    db.close()

  /** The query to initialize the repo. */
  private def initQuery: DBIO[Unit] = {
    // Initialize schema suppressing errors. Workaround for slick/slick#1999.
    FileVersions.schema.createIfNotExists.asTry >> DBIO.successful(())
  }

  /** The query to clean the repo. */
  private def cleanQuery: DBIO[Unit] =
    FileVersions.delete >> DBIO.successful(())

  /** The query to get the version digest of the file.
    *
    * @param file the file path
    * @return the version digest
    */
  private def getVersionQuery(file: File): DBIO[Option[Array[Byte]]] = {
    val query = for {
      row <- FileVersions
      if row.path === file.toString
    } yield row.digest
    query.result.headOption
  }

  /** The query to set the version digest of the file.
    *
    * @param file the file path
    * @param version the version digest
    * @return the previously recorded vile version
    */
  private def setVersionQuery(
    file: File,
    version: Array[Byte]
  ): DBIO[Option[Array[Byte]]] = {
    val upsertQuery = FileVersions
      .insertOrUpdate(FileVersionRow(file.toString, version))
    for {
      version <- getVersionQuery(file)
      _       <- upsertQuery
    } yield version
  }

  /** The query to update the version if it differs from the recorded version.
    *
    * @param file the file path
    * @param version the version digest
    * @return `true` if the version has been updated
    */
  private def updateVersionQuery(
    file: File,
    version: Array[Byte]
  ): DBIO[Boolean] =
    for {
      repoVersion <- getVersionQuery(file)
      versionsEquals = repoVersion.fold(false)(compareVersions(_, version))
      _ <-
        if (!versionsEquals) setVersionQuery(file, version)
        else DBIO.successful(None)
    } yield !versionsEquals


  /** The query to update the versions in batch.
    *
    * @param versions files with corresponding digests
    */
  private def updateVersionsQuery(
    versions: Seq[(File, Array[Byte])]
  ): DBIO[Unit] =
    if (versions.nonEmpty) {
      def upsertQuery(file: File, version: Array[Byte]) = FileVersions
        .insertOrUpdate(FileVersionRow(file.toString, version))
      DBIO.sequence(versions.map(Function.tupled(upsertQuery))) >>
      DBIO.successful(())
    } else {
      DBIO.successful(())
    }

  /** The query to remove the version record.
    *
    * @param file the file path
    */
  private def removeQuery(file: File): DBIO[Unit] = {
    val query = for {
      row <- FileVersions
      if row.path === file.toString
    } yield row
    query.delete >> DBIO.successful(())
  }

  private def compareVersions(v1: Array[Byte], v2: Array[Byte]): Boolean =
    util.Arrays.equals(v1, v2)

}

object SqlVersionsRepo {

  /** Create the in-memory file versions repo.
    *
    * @return the versions repo backed up by SQL database
    */
  def apply()(implicit ec: ExecutionContext): SqlVersionsRepo = {
    new SqlVersionsRepo(new SqlDatabase())
  }

  /** Create the file versions repo.
    *
    * @param path the path to the database file
    * @return the file versions repo backed up by SQL database
    */
  def apply(path: File)(implicit ec: ExecutionContext): SqlVersionsRepo = {
    new SqlVersionsRepo(SqlDatabase(path.toString))
  }
}
