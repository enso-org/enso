package org.enso.searcher.sql

import org.enso.searcher.{Suggestion, SuggestionEntry, SuggestionsRepo}
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/** The object for accessing the suggestions database. */
final private[sql] class SuggestionsDbio(implicit ec: ExecutionContext)
    extends SuggestionsRepo[DBIO] {

  /** The query returning the arguments joined with the corresponding
    * suggestions.
    */
  private val joined: Query[
    (Rep[Option[ArgumentsTable]], SuggestionsTable),
    (Option[ArgumentRow], SuggestionRow),
    Seq
  ] =
    Arguments
      .joinRight(Suggestions)
      .on(_.suggestionId === _.id)

  /** Initialize the repo. */
  override def init: DBIO[Unit] =
    (Suggestions.schema ++ Arguments.schema ++ Versions.schema).createIfNotExists

  /** Clean the repo. */
  override def clean: DBIO[Unit] =
    for {
      _ <- Suggestions.delete
      _ <- Arguments.delete
      _ <- Versions.delete
    } yield ()

  /** Get all suggestions. */
  override def getAll: DBIO[(Long, Seq[SuggestionEntry])] = {
    val query = for {
      suggestions <- joined.result.map(joinedToSuggestionEntries)
      version     <- currentVersion
    } yield (version, suggestions)
    query.transactionally
  }

  /** Search suggestion by various parameters.
    *
    * @param selfType the selfType search parameter
    * @param returnType the returnType search parameter
    * @param kinds the list suggestion kinds to search
    * @return the list of suggestion ids
    */
  override def search(
    selfType: Option[String],
    returnType: Option[String],
    kinds: Option[Seq[Suggestion.Kind]]
  ): DBIO[(Long, Seq[Long])] = {
    val searchAction =
      if (selfType.isEmpty && returnType.isEmpty && kinds.isEmpty) {
        DBIO.successful(Seq())
      } else {
        val query = searchQuery(selfType, returnType, kinds).map(_.id)
        query.result
      }
    val query = for {
      results <- searchAction
      version <- currentVersion
    } yield (version, results)
    query.transactionally
  }

  /** Select the suggestion by id.
    *
    * @param id the id of a suggestion
    * @return return the suggestion
    */
  override def select(id: Long): DBIO[Option[Suggestion]] = {
    val query = for {
      (argument, suggestion) <- joined
      if suggestion.id === id
    } yield (argument, suggestion)
    query.result.map(coll => joinedToSuggestions(coll).headOption)
  }

  /** Insert the suggestion
    *
    * @param suggestion the suggestion to insert
    * @return the id of an inserted suggestion
    */
  override def insert(suggestion: Suggestion): DBIO[Option[Long]] = {
    val (suggestionRow, args) = toSuggestionRow(suggestion)
    val query = for {
      id <- Suggestions.returning(Suggestions.map(_.id)) += suggestionRow
      _ <- Arguments ++= args.zipWithIndex.map {
          case (argument, ix) => toArgumentRow(id, ix, argument)
        }
      _ <- incrementVersion
    } yield id
    query.transactionally.asTry.map {
      case Failure(_)  => None
      case Success(id) => Some(id)
    }
  }

  /** Insert a list of suggestions
    *
    * @param suggestions the suggestions to insert
    * @return the list of inserted suggestion ids
    */
  override def insertAll(
    suggestions: Seq[Suggestion]
  ): DBIO[(Long, Seq[Option[Long]])] = {
    val query = for {
      ids     <- DBIO.sequence(suggestions.map(insert))
      version <- currentVersion
    } yield (version, ids)
    query.transactionally
  }

  /** Remove the suggestion.
    *
    * @param suggestion the suggestion to remove
    * @return the id of removed suggestion
    */
  override def remove(suggestion: Suggestion): DBIO[Option[Long]] = {
    val (raw, _) = toSuggestionRow(suggestion)
    val selectQuery = Suggestions
      .filter(_.kind === raw.kind)
      .filter(_.name === raw.name)
      .filter(_.scopeStart === raw.scopeStart)
      .filter(_.scopeEnd === raw.scopeEnd)
    val deleteQuery = for {
      rows <- selectQuery.result
      n    <- selectQuery.delete
      _    <- if (n > 0) incrementVersion else DBIO.successful(())
    } yield rows.flatMap(_.id).headOption
    deleteQuery.transactionally
  }

  /** Remove a list of suggestions.
    *
    * @param suggestions the suggestions to remove
    * @return the list of removed suggestion ids
    */
  override def removeAll(
    suggestions: Seq[Suggestion]
  ): DBIO[(Long, Seq[Option[Long]])] = {
    val query = for {
      ids     <- DBIO.sequence(suggestions.map(remove))
      version <- currentVersion
    } yield (version, ids)
    query.transactionally
  }

  /** Get current version of the repo. */
  override def currentVersion: DBIO[Long] = {
    for {
      versionOpt <- Versions.result.headOption
    } yield versionOpt.flatMap(_.id).getOrElse(0L)
  }

  /** Increment the current version of the repo. */
  private def incrementVersion: DBIO[Long] = {
    val increment = for {
      version <- Versions.returning(Versions.map(_.id)) += VersionRow(None)
      _       <- Versions.filterNot(_.id === version).delete
    } yield version
    increment.transactionally
  }

  /** Insert suggestions in a batch. */
  private[sql] def insertBatch(suggestions: Array[Suggestion]): DBIO[Int] = {
    val rows = suggestions.map(toSuggestionRow)
    for {
      _    <- (Suggestions ++= rows.map(_._1)).asTry
      size <- Suggestions.length.result
    } yield size
  }

  /** Create a search query by the provided parameters. */
  private def searchQuery(
    selfType: Option[String],
    returnType: Option[String],
    kinds: Option[Seq[Suggestion.Kind]]
  ): Query[SuggestionsTable, SuggestionRow, Seq] = {
    Suggestions
      .filterOpt(selfType) {
        case (row, value) => row.selfType === value
      }
      .filterOpt(returnType) {
        case (row, value) => row.returnType === value
      }
      .filterOpt(kinds) {
        case (row, value) => row.kind inSet value.map(SuggestionKind(_))
      }
  }

  /** Convert the rows of suggestions joined with arguments to a list of
    * suggestions.
    */
  private def joinedToSuggestions(
    coll: Seq[(Option[ArgumentRow], SuggestionRow)]
  ): Seq[Suggestion] = {
    coll
      .groupBy(_._2)
      .view
      .mapValues(_.flatMap(_._1))
      .map(Function.tupled(toSuggestion))
      .toSeq
  }

  /** Convert the rows of suggestions joined with arguments to a list of
    * suggestion entries.
    */
  private def joinedToSuggestionEntries(
    coll: Seq[(Option[ArgumentRow], SuggestionRow)]
  ): Seq[SuggestionEntry] = {
    coll
      .groupBy(_._2)
      .view
      .mapValues(_.flatMap(_._1))
      .map(Function.tupled(toSuggestionEntry))
      .toSeq
  }

  /** Convert the suggestion to a row in the suggestions table. */
  private def toSuggestionRow(
    suggestion: Suggestion
  ): (SuggestionRow, Seq[Suggestion.Argument]) =
    suggestion match {
      case Suggestion.Atom(name, args, returnType, doc) =>
        val row = SuggestionRow(
          id            = None,
          kind          = SuggestionKind.ATOM,
          name          = name,
          selfType      = None,
          returnType    = returnType,
          documentation = doc,
          scopeStart    = ScopeColumn.EMPTY,
          scopeEnd      = ScopeColumn.EMPTY
        )
        row -> args
      case Suggestion.Method(name, args, selfType, returnType, doc) =>
        val row = SuggestionRow(
          id            = None,
          kind          = SuggestionKind.METHOD,
          name          = name,
          selfType      = Some(selfType),
          returnType    = returnType,
          documentation = doc,
          scopeStart    = ScopeColumn.EMPTY,
          scopeEnd      = ScopeColumn.EMPTY
        )
        row -> args
      case Suggestion.Function(name, args, returnType, scope) =>
        val row = SuggestionRow(
          id            = None,
          kind          = SuggestionKind.FUNCTION,
          name          = name,
          selfType      = None,
          returnType    = returnType,
          documentation = None,
          scopeStart    = scope.start,
          scopeEnd      = scope.end
        )
        row -> args
      case Suggestion.Local(name, returnType, scope) =>
        val row = SuggestionRow(
          id            = None,
          kind          = SuggestionKind.LOCAL,
          name          = name,
          selfType      = None,
          returnType    = returnType,
          documentation = None,
          scopeStart    = scope.start,
          scopeEnd      = scope.end
        )
        row -> Seq()
    }

  /** Convert the argument to a row in the arguments table. */
  private def toArgumentRow(
    suggestionId: Long,
    index: Int,
    argument: Suggestion.Argument
  ): ArgumentRow =
    ArgumentRow(
      id           = None,
      suggestionId = suggestionId,
      index        = index,
      name         = argument.name,
      tpe          = argument.reprType,
      isSuspended  = argument.isSuspended,
      hasDefault   = argument.hasDefault,
      defaultValue = argument.defaultValue
    )

  /** Convert the database rows to a suggestion entry. */
  private def toSuggestionEntry(
    suggestion: SuggestionRow,
    arguments: Seq[ArgumentRow]
  ): SuggestionEntry =
    SuggestionEntry(suggestion.id.get, toSuggestion(suggestion, arguments))

  /** Convert the databaes rows to a suggestion. */
  private def toSuggestion(
    suggestion: SuggestionRow,
    arguments: Seq[ArgumentRow]
  ): Suggestion =
    suggestion.kind match {
      case SuggestionKind.ATOM =>
        Suggestion.Atom(
          name          = suggestion.name,
          arguments     = arguments.sortBy(_.index).map(toArgument),
          returnType    = suggestion.returnType,
          documentation = suggestion.documentation
        )
      case SuggestionKind.METHOD =>
        Suggestion.Method(
          name          = suggestion.name,
          arguments     = arguments.sortBy(_.index).map(toArgument),
          selfType      = suggestion.selfType.get,
          returnType    = suggestion.returnType,
          documentation = suggestion.documentation
        )
      case SuggestionKind.FUNCTION =>
        Suggestion.Function(
          name       = suggestion.name,
          arguments  = arguments.sortBy(_.index).map(toArgument),
          returnType = suggestion.returnType,
          scope      = Suggestion.Scope(suggestion.scopeStart, suggestion.scopeEnd)
        )
      case SuggestionKind.LOCAL =>
        Suggestion.Local(
          name       = suggestion.name,
          returnType = suggestion.returnType,
          scope      = Suggestion.Scope(suggestion.scopeStart, suggestion.scopeEnd)
        )

      case k =>
        throw new NoSuchElementException(s"Unknown suggestion kind: $k")
    }

  /** Convert the database row to the suggestion argument. */
  private def toArgument(row: ArgumentRow): Suggestion.Argument =
    Suggestion.Argument(
      name         = row.name,
      reprType     = row.tpe,
      isSuspended  = row.isSuspended,
      hasDefault   = row.hasDefault,
      defaultValue = row.defaultValue
    )
}

private[sql] object SuggestionsDbio {

  /** Create the [[SuggestionsDbio]] instance. */
  def apply()(implicit ec: ExecutionContext): SuggestionsDbio =
    new SuggestionsDbio()
}
