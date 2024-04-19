package org.enso.searcher.memory

import org.enso.polyglot.Suggestion
import org.enso.polyglot.Suggestion.ExternalID
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.searcher.data.QueryResult
import org.enso.searcher.sql.SuggestionRowUniqueIndex
import org.enso.searcher.{SuggestionEntry, SuggestionsRepo}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

class InmemorySuggestionsRepo(implicit ec: ExecutionContext)
    extends SuggestionsRepo[Future] {
  private[this] var db: mutable.Map[Long, Suggestion] = null
  @volatile private[this] var version: Long           = 0
  @volatile private[this] var index: Long             = 0

  /** Initialize the repo. */
  override def init: Future[Unit] = {
    Future {
      if (db == null) {
        db = new mutable.HashMap()
      }
    }
  }

  /** Get current version of the repo. */
  override def currentVersion: Future[Long] = {
    Future {
      version
    }
  }

  /** Get all suggestions.
    *
    * @return the current database version and the list of suggestions
    */
  override def getAll: Future[(Long, Seq[SuggestionEntry])] = {
    Future {
      db.synchronized {
        (version, db.toSeq.map(v => SuggestionEntry(v._1, v._2)))
      }
    }
  }

  /** Select the suggestion by id.
    *
    * @param id the id of a suggestion
    * @return return the suggestion
    */
  override def select(id: Long): Future[Option[Suggestion]] = Future {
    db.synchronized {
      db.get(id)
    }
  }

  /** Insert the suggestion.
    *
    * @param suggestion the suggestion to insert
    * @return the id of an inserted suggestion
    */
  override def insert(suggestion: Suggestion): Future[Option[Long]] = Future {
    db.synchronized {
      val i = index
      index += 1
      db.put(i, suggestion)
      Some(i)
    }
  }

  /** Insert a list of suggestions.
    *
    * @param suggestions the suggestions to insert
    * @return the current database version and a list of inserted suggestion ids
    */
  override def insertAll(
    suggestions: Seq[Suggestion]
  ): Future[(Long, Seq[Long])] = Future {
    val duplicatesBuilder = Vector.newBuilder[(Suggestion, Suggestion)]
    val suggestionsMap: mutable.Map[SuggestionRowUniqueIndex, Suggestion] =
      mutable.LinkedHashMap()
    suggestions.foreach { suggestion =>
      val idx = SuggestionRowUniqueIndex(suggestion)
      suggestionsMap.put(idx, suggestion).foreach { duplicate =>
        duplicatesBuilder.addOne((duplicate, suggestion))
      }
    }
    val duplicates = duplicatesBuilder.result()
    if (duplicates.isEmpty) {
      db.synchronized {
        val result = suggestions.map(s => {
          val i = index
          index += 1
          db.put(i, s)
          i
        })
        version += 1
        (version, result)
      }
    } else {
      throw new RuntimeException("Duplicates detected: " + duplicates)
    }
    db.synchronized {
      val result = suggestions.map(s => {
        val i = index
        index += 1
        db.put(i, s)
        i
      })
      version += 1
      (version, result)
    }
  }

  /** Apply suggestion updates.
    *
    * @param tree the sequence of suggestion updates
    * @return the result of applying the updates
    */
  override def applyTree(
    tree: Seq[Api.SuggestionUpdate]
  ): Future[Seq[QueryResult[Api.SuggestionUpdate]]] = Future {
    db.synchronized {
      ???
    }
  }

  /** Apply the sequence of actions on the database.
    *
    * @param actions the list of actions
    * @return the result of applying the actions
    */
  override def applyActions(
    actions: Seq[Api.SuggestionsDatabaseAction]
  ): Future[Seq[QueryResult[Api.SuggestionsDatabaseAction]]] = ???

  /** Get the suggestions related to the export updates.
    *
    * @param actions the list of updates
    * @return the suggestions ids associated with the export updates
    */
  override def getExportedSymbols(
    actions: Seq[Api.ExportsUpdate]
  ): Future[Seq[QueryResult[Api.ExportsUpdate]]] = ???

  /** Remove the suggestion.
    *
    * @param suggestion the suggestion to remove
    * @return the id of removed suggestion
    */
  override def remove(suggestion: Suggestion): Future[Option[Long]] = ???

  /** Remove suggestions by module names.
    *
    * @param modules the list of module names
    * @return the current database version and a list of removed suggestion ids
    */
  override def removeModules(modules: Seq[String]): Future[(Long, Seq[Long])] =
    ???

  /** Update the suggestion.
    *
    * @param suggestion    the key suggestion
    * @param externalId    the external id to update
    * @param returnType    the return type to update
    * @param documentation the documentation string to update
    * @param scope         the scope to update
    */
  override def update(
    suggestion: Suggestion,
    externalId: Option[Option[ExternalID]],
    returnType: Option[String],
    documentation: Option[Option[String]],
    scope: Option[Suggestion.Scope]
  ): Future[(Long, Option[Long])] = ???

  /** Update a list of suggestions by external id.
    *
    * @param expressions pairs of external id and a return type
    * @return the current database version and a list of updated suggestion ids
    */
  override def updateAll(
    expressions: Seq[(ExternalID, String)]
  ): Future[(Long, Seq[Option[Long]])] = Future {
    db.synchronized {
      // TODO: consider adding an index for `externalID`
      val result = expressions.map { case (externalID, expr) =>
        db.find(e => externalIDMatches(e._2.externalId, externalID)).map {
          case (idx, s) =>
            val v = s.withReturnType(expr)
            db.put(idx, v)
            idx
        }
      }
      condVersionIncrement(result.find(_.nonEmpty).nonEmpty)
      (version, result)
    }
  }

  private def condVersionIncrement(cond: => Boolean): Unit = {
    if (cond) version += 1
  }

  private def externalIDMatches(
    existing: Option[ExternalID],
    externalID: ExternalID
  ): Boolean = {
    existing
      .map(e =>
        e.getLeastSignificantBits == externalID.getLeastSignificantBits && e.getMostSignificantBits == externalID.getMostSignificantBits
      )
      .getOrElse(false)
  }

  /** Cleans the repo resetting the version. */
  override def clean: Future[Unit] = Future {
    if (db != null) {
      db.synchronized {
        db.clear()
      }
    }
  }

  def close(): Unit = {}
}
