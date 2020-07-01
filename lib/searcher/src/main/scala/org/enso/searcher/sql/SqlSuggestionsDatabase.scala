package org.enso.searcher.sql

import org.enso.searcher.{Suggestion, SuggestionEntry, SuggestionsRepo}
import slick.jdbc.SQLiteProfile.api._
import slick.jdbc.TransactionIsolation

import scala.concurrent.{ExecutionContext, Future}

/** The object for accessing the suggestions database. */
final class SqlSuggestionsDatabase(
  db: SqlDatabase,
  repo: SqlSuggestionsRepo
) extends SuggestionsRepo[Future] {

  override def init: Future[Unit] =
    db.run(repo.init)

  /** @inheritdoc */
  override def getAll: Future[Seq[SuggestionEntry]] =
    db.run(repo.getAll)

  /** @inheritdoc */
  override def search(
    selfType: Option[String],
    returnType: Option[String],
    kinds: Option[Seq[Suggestion.Kind]]
  ): Future[Seq[Long]] =
    db.run(repo.search(selfType, returnType, kinds))

  /** @inheritdoc */
  override def select(id: Long): Future[Option[Suggestion]] =
    db.run(repo.select(id))

  /** @inheritdoc */
  override def insert(suggestion: Suggestion): Future[Option[Long]] =
    db.run(repo.insert(suggestion))

  /** @inheritdoc */
  override def insertAll(
    suggestions: Seq[Suggestion]
  ): Future[Seq[Option[Long]]] =
    db.run(repo.insertAll(suggestions))

  /** @inheritdoc */
  override def remove(suggestion: Suggestion): Future[Option[Long]] =
    db.run(
      repo
        .remove(suggestion)
        .withTransactionIsolation(TransactionIsolation.RepeatableRead)
    )

  /** @inheritdoc */
  override def removeAll(
    suggestions: Seq[Suggestion]
  ): Future[Seq[Option[Long]]] =
    db.run(repo.removeAll(suggestions))

  /** @inheritdoc */
  override def currentVersion: Future[Long] =
    db.run(repo.currentVersion)
}

object SqlSuggestionsDatabase {

  /** Create the suggestions database.
    *
    * @param filename the database filename
    * @param ec the execution context
    * @return new instance of suggestions database.
    */
  def apply(
    filename: String = SqlDatabase.InMemory
  )(implicit ec: ExecutionContext): SqlSuggestionsDatabase =
    new SqlSuggestionsDatabase(
      new SqlDatabase(filename),
      new SqlSuggestionsRepo()
    )
}
