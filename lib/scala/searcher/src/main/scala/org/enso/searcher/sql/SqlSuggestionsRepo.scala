package org.enso.searcher.sql

import java.nio.file.Path

import org.enso.searcher.{Suggestion, SuggestionEntry, SuggestionsRepo}

import scala.concurrent.{ExecutionContext, Future}

/** The object for accessing the suggestions database. */
final class SqlSuggestionsRepo private (db: SqlDatabase, dbio: SuggestionsDbio)
    extends SuggestionsRepo[Future] {

  /** @inheritdoc */
  override def init: Future[Unit] =
    db.run(dbio.init)

  /** @inheritdoc */
  override def clean: Future[Unit] =
    db.run(dbio.clean)

  /** @inheritdoc */
  override def getAll: Future[(Long, Seq[SuggestionEntry])] =
    db.run(dbio.getAll)

  /** @inheritdoc */
  override def search(
    selfType: Option[String],
    returnType: Option[String],
    kinds: Option[Seq[Suggestion.Kind]]
  ): Future[(Long, Seq[Long])] =
    db.run(dbio.search(selfType, returnType, kinds))

  /** @inheritdoc */
  override def select(id: Long): Future[Option[Suggestion]] =
    db.run(dbio.select(id))

  /** @inheritdoc */
  override def insert(suggestion: Suggestion): Future[Option[Long]] =
    db.run(dbio.insert(suggestion))

  /** @inheritdoc */
  override def insertAll(
    suggestions: Seq[Suggestion]
  ): Future[(Long, Seq[Option[Long]])] =
    db.run(dbio.insertAll(suggestions))

  /** @inheritdoc */
  override def remove(suggestion: Suggestion): Future[Option[Long]] =
    db.run(dbio.remove(suggestion))

  /** @inheritdoc */
  override def removeAll(
    suggestions: Seq[Suggestion]
  ): Future[(Long, Seq[Option[Long]])] =
    db.run(dbio.removeAll(suggestions))

  /** @inheritdoc */
  override def currentVersion: Future[Long] =
    db.run(dbio.currentVersion)

  /** Close the database. */
  def close(): Unit =
    db.close()
}

object SqlSuggestionsRepo {

  /** Create the suggestions repo.
    *
    * @return the suggestions repo backed up by SQL database.
    */
  def apply()(implicit ec: ExecutionContext): SqlSuggestionsRepo = {
    new SqlSuggestionsRepo(
      db   = new SqlDatabase(),
      dbio = new SuggestionsDbio()
    )
  }

  /** Create the suggestions repo.
    *
    * @param path the path to the database file.
    * @return the suggestions repo backed up by SQL database.
    */
  def apply(path: Path)(implicit ec: ExecutionContext): SqlSuggestionsRepo = {
    new SqlSuggestionsRepo(
      db   = SqlDatabase(path.toString),
      dbio = SuggestionsDbio()
    )
  }
}
