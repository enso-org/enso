package org.enso.searcher.sql

import java.io.File
import java.util.UUID

import org.enso.polyglot.Suggestion
import org.enso.searcher.{SuggestionEntry, SuggestionsRepo}
import slick.jdbc.SQLiteProfile
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** The object for accessing the suggestions database. */
final class SqlSuggestionsRepo(db: SqlDatabase)(implicit ec: ExecutionContext)
    extends SuggestionsRepo[Future] {

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
  def init: Future[Unit] =
    db.run(initQuery)

  /** @inheritdoc */
  override def clean: Future[Unit] =
    db.run(cleanQuery)

  /** @inheritdoc */
  override def getAll: Future[(Long, Seq[SuggestionEntry])] =
    db.run(getAllQuery)

  /** @inheritdoc */
  override def getAllMethods(
    calls: Seq[(String, String, String)]
  ): Future[Seq[Option[Long]]] =
    db.run(getAllMethodsQuery(calls))

  /** @inheritdoc */
  override def search(
    module: Option[String],
    selfType: Option[String],
    returnType: Option[String],
    kinds: Option[Seq[Suggestion.Kind]],
    position: Option[Suggestion.Position]
  ): Future[(Long, Seq[Long])] =
    db.run(searchQuery(module, selfType, returnType, kinds, position))

  /** @inheritdoc */
  override def select(id: Long): Future[Option[Suggestion]] =
    db.run(selectQuery(id))

  /** @inheritdoc */
  override def insert(suggestion: Suggestion): Future[Option[Long]] =
    db.run(insertQuery(suggestion))

  /** @inheritdoc */
  override def insertAll(
    suggestions: Seq[Suggestion]
  ): Future[(Long, Seq[Option[Long]])] =
    db.run(insertAllQuery(suggestions))

  /** @inheritdoc */
  override def remove(suggestion: Suggestion): Future[Option[Long]] =
    db.run(removeQuery(suggestion))

  /** @inheritdoc */
  override def removeByModule(name: String): Future[(Long, Seq[Long])] =
    db.run(removeByModuleQuery(name))

  /** @inheritdoc */
  override def removeAllByModule(
    modules: Seq[String]
  ): Future[(Long, Seq[Long])] =
    db.run(removeAllByModuleQuery(modules))

  /** @inheritdoc */
  override def removeAll(
    suggestions: Seq[Suggestion]
  ): Future[(Long, Seq[Option[Long]])] =
    db.run(removeAllQuery(suggestions))

  /** @inheritdoc */
  override def updateAll(
    expressions: Seq[(Suggestion.ExternalId, String)]
  ): Future[(Long, Seq[Option[Long]])] =
    db.run(updateAllQuery(expressions))

  /** @inheritdoc */
  override def renameProject(oldName: String, newName: String): Future[Unit] =
    db.run(renameProjectQuery(oldName, newName))

  /** @inheritdoc */
  override def currentVersion: Future[Long] =
    db.run(currentVersionQuery)

  /** Close the database. */
  def close(): Unit =
    db.close()

  /** Insert suggestions in a batch.
    *
    * @param suggestions the list of suggestions to insert
    * @return the current database size
    */
  private[sql] def insertBatch(suggestions: Array[Suggestion]): Future[Int] =
    db.run(insertBatchQuery(suggestions))

  /** The query to initialize the repo. */
  private def initQuery: DBIO[Unit] = {
    // Initialize schema suppressing errors. Workaround for slick/slick#1999.
    def initSchema(schema: SQLiteProfile.SchemaDescription) =
      schema.createIfNotExists.asTry >> DBIO.successful(())
    val schemas =
      Seq(Suggestions.schema, Arguments.schema, SuggestionsVersions.schema)
    DBIO.sequence(schemas.map(initSchema)) >> DBIO.successful(())
  }

  /** The query to clean the repo. */
  private def cleanQuery: DBIO[Unit] = {
    for {
      _ <- Suggestions.delete
      _ <- Arguments.delete
      _ <- SuggestionsVersions.delete
    } yield ()
  }

  /** Get all suggestions.
    *
    * @return the current database version with the list of suggestion entries
    */
  private def getAllQuery: DBIO[(Long, Seq[SuggestionEntry])] = {
    val query = for {
      suggestions <- joined.result.map(joinedToSuggestionEntries)
      version     <- currentVersionQuery
    } yield (version, suggestions)
    query
  }

  /** The query to get the suggestions by the method call info.
    *
    * @param calls the triples containing module name, self type, method name
    * @return the list of found suggestion ids
    */
  def getAllMethodsQuery(
    calls: Seq[(String, String, String)]
  ): DBIO[Seq[Option[Long]]] =
    if (calls.isEmpty) {
      DBIO.successful(Seq())
    } else {
      val query = Suggestions
        .filter { row =>
          calls
            .map {
              case (module, selfType, name) =>
                row.module === module && row.selfType === selfType && row.name === name
            }
            .reduce(_ || _)
        }
        .map(row => (row.id, row.module, row.selfType, row.name))
      query.result.map { tuples =>
        val result = tuples.map {
          case (id, module, selfType, name) => (module, selfType, name) -> id
        }.toMap
        calls.map(result.get)
      }
    }

  /** The query to search suggestion by various parameters.
    *
    * @param module the module name search parameter
    * @param selfType the selfType search parameter
    * @param returnType the returnType search parameter
    * @param kinds the list suggestion kinds to search
    * @param position the absolute position in the text
    * @return the list of suggestion ids
    */
  private def searchQuery(
    module: Option[String],
    selfType: Option[String],
    returnType: Option[String],
    kinds: Option[Seq[Suggestion.Kind]],
    position: Option[Suggestion.Position]
  ): DBIO[(Long, Seq[Long])] = {
    val searchAction =
      if (
        module.isEmpty &&
        selfType.isEmpty &&
        returnType.isEmpty &&
        kinds.isEmpty &&
        position.isEmpty
      ) {
        DBIO.successful(Seq())
      } else {
        val query =
          searchQueryBuilder(module, selfType, returnType, kinds, position)
            .map(_.id)
        query.result
      }
    val query = for {
      results <- searchAction
      version <- currentVersionQuery
    } yield (version, results)
    query
  }

  /** The query to select the suggestion by id.
    *
    * @param id the id of a suggestion
    * @return return the suggestion
    */
  private def selectQuery(id: Long): DBIO[Option[Suggestion]] = {
    val query = for {
      (argument, suggestion) <- joined
      if suggestion.id === id
    } yield (argument, suggestion)
    query.result.map(coll => joinedToSuggestions(coll).headOption)
  }

  /** The query to insert the suggestion
    *
    * @param suggestion the suggestion to insert
    * @return the id of an inserted suggestion
    */
  private def insertQuery(suggestion: Suggestion): DBIO[Option[Long]] = {
    val (suggestionRow, args) = toSuggestionRow(suggestion)
    val query = for {
      id <- Suggestions.returning(Suggestions.map(_.id)) += suggestionRow
      _ <- Arguments ++= args.zipWithIndex.map {
          case (argument, ix) => toArgumentRow(id, ix, argument)
        }
      _ <- incrementVersionQuery
    } yield id
    query.asTry.map {
      case Failure(_)  => None
      case Success(id) => Some(id)
    }
  }

  /** The query to insert a list of suggestions
    *
    * @param suggestions the suggestions to insert
    * @return the list of inserted suggestion ids
    */
  private def insertAllQuery(
    suggestions: Seq[Suggestion]
  ): DBIO[(Long, Seq[Option[Long]])] = {
    val query = for {
      ids     <- DBIO.sequence(suggestions.map(insertQuery))
      version <- currentVersionQuery
    } yield (version, ids)
    query
  }

  /** The query to remove the suggestion.
    *
    * @param suggestion the suggestion to remove
    * @return the id of removed suggestion
    */
  private def removeQuery(suggestion: Suggestion): DBIO[Option[Long]] = {
    val (raw, _) = toSuggestionRow(suggestion)
    val selectQuery = Suggestions
      .filter(_.kind === raw.kind)
      .filter(_.name === raw.name)
      .filter(_.scopeStartLine === raw.scopeStartLine)
      .filter(_.scopeStartOffset === raw.scopeStartOffset)
      .filter(_.scopeEndLine === raw.scopeEndLine)
      .filter(_.scopeEndOffset === raw.scopeEndOffset)
    val deleteQuery = for {
      rows <- selectQuery.result
      n    <- selectQuery.delete
      _    <- if (n > 0) incrementVersionQuery else DBIO.successful(())
    } yield rows.flatMap(_.id).headOption
    deleteQuery
  }

  /** The query to remove the suggestions by module name
    *
    * @param name the module name
    * @return the current database version and a list of removed suggestion ids
    */
  private def removeByModuleQuery(name: String): DBIO[(Long, Seq[Long])] = {
    val selectQuery = Suggestions.filter(_.module === name)
    val deleteQuery = for {
      rows    <- selectQuery.result
      n       <- selectQuery.delete
      version <- if (n > 0) incrementVersionQuery else currentVersionQuery
    } yield version -> rows.flatMap(_.id)
    deleteQuery
  }

  /** The query to remove all suggestions by module names.
    *
    * @param modules the list of modules to remove
    * @return the current database version and a list of removed suggestion ids
    */
  private def removeAllByModuleQuery(
    modules: Seq[String]
  ): DBIO[(Long, Seq[Long])] = {
    val selectQuery = Suggestions.filter(_.module inSet modules)
    val deleteQuery = for {
      rows    <- selectQuery.result
      n       <- selectQuery.delete
      version <- if (n > 0) incrementVersionQuery else currentVersionQuery
    } yield version -> rows.flatMap(_.id)
    deleteQuery
  }

  /** The query to remove a list of suggestions.
    *
    * @param suggestions the suggestions to remove
    * @return the list of removed suggestion ids
    */
  private def removeAllQuery(
    suggestions: Seq[Suggestion]
  ): DBIO[(Long, Seq[Option[Long]])] = {
    val query = for {
      ids     <- DBIO.sequence(suggestions.map(removeQuery))
      version <- currentVersionQuery
    } yield (version, ids)
    query
  }

  /** The query to update a suggestion.
    *
    * @param externalId the external id of a suggestion
    * @param returnType the new return type
    * @return the id of updated suggestion
    */
  private def updateQuery(
    externalId: Suggestion.ExternalId,
    returnType: String
  ): DBIO[Option[Long]] = {
    val selectQuery = Suggestions
      .filter { row =>
        row.externalIdLeast === externalId.getLeastSignificantBits &&
        row.externalIdMost === externalId.getMostSignificantBits
      }
    for {
      id <- selectQuery.map(_.id).result.headOption
      _  <- selectQuery.map(_.returnType).update(returnType)
    } yield id
  }

  /** The query to update a list of suggestions by external id.
    *
    * @param expressions the list of expressions to update
    * @return the current database version with the list of updated suggestion ids
    */
  private def updateAllQuery(
    expressions: Seq[(Suggestion.ExternalId, String)]
  ): DBIO[(Long, Seq[Option[Long]])] = {
    val query = for {
      ids <- DBIO.sequence(expressions.map(Function.tupled(updateQuery)))
      version <-
        if (ids.exists(_.nonEmpty)) incrementVersionQuery
        else currentVersionQuery
    } yield (version, ids)
    query
  }

  /** The query to update the project name.
    *
    * @param oldName the old name of the project
    * @param newName the new project name
    */
  private def renameProjectQuery(
    oldName: String,
    newName: String
  ): DBIO[Unit] = {
    val updateQuery =
      sqlu"""update suggestions
          set module = replace(module, $oldName, $newName)
          where module like '#$oldName%'"""
    updateQuery >> DBIO.successful(())
  }

  /** The query to get current version of the repo. */
  private def currentVersionQuery: DBIO[Long] = {
    for {
      versionOpt <- SuggestionsVersions.result.headOption
    } yield versionOpt.flatMap(_.id).getOrElse(0L)
  }

  /** The query to increment the current version of the repo. */
  private def incrementVersionQuery: DBIO[Long] = {
    val increment = for {
      version <- SuggestionsVersions.returning(
        SuggestionsVersions.map(_.id)
      ) += SuggestionsVersionRow(None)
      _ <- SuggestionsVersions.filterNot(_.id === version).delete
    } yield version
    increment.transactionally
  }

  /** The query to insert suggestions in a batch.
    *
    * @param suggestions the list of suggestions to insert
    * @return the current size of the database
    */
  private def insertBatchQuery(
    suggestions: Array[Suggestion]
  ): DBIO[Int] = {
    val rows = suggestions.map(toSuggestionRow)
    for {
      _    <- (Suggestions ++= rows.map(_._1)).asTry
      size <- Suggestions.length.result
    } yield size
  }

  /** Create a search query by the provided parameters.
    *
    * @param module the module name search parameter
    * @param selfType the selfType search parameter
    * @param returnType the returnType search parameter
    * @param kinds the list suggestion kinds to search
    * @param position the absolute position in the text
    * @return the search query
    */
  private def searchQueryBuilder(
    module: Option[String],
    selfType: Option[String],
    returnType: Option[String],
    kinds: Option[Seq[Suggestion.Kind]],
    position: Option[Suggestion.Position]
  ): Query[SuggestionsTable, SuggestionRow, Seq] = {
    Suggestions
      .filterOpt(module) {
        case (row, value) => row.module === value
      }
      .filterOpt(selfType) {
        case (row, value) => row.selfType === value
      }
      .filterOpt(returnType) {
        case (row, value) => row.returnType === value
      }
      .filterOpt(kinds) {
        case (row, value) => row.kind inSet value.map(SuggestionKind(_))
      }
      .filterOpt(position) {
        case (row, value) =>
          (row.scopeStartLine === ScopeColumn.EMPTY) ||
          (
            row.scopeStartLine <= value.line &&
            row.scopeStartOffset <= value.character &&
            row.scopeEndLine >= value.line &&
            row.scopeEndOffset >= value.character
          )
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
      case Suggestion.Atom(expr, module, name, args, returnType, doc) =>
        val row = SuggestionRow(
          id               = None,
          externalIdLeast  = expr.map(_.getLeastSignificantBits),
          externalIdMost   = expr.map(_.getMostSignificantBits),
          kind             = SuggestionKind.ATOM,
          module           = module,
          name             = name,
          selfType         = SelfTypeColumn.EMPTY,
          returnType       = returnType,
          documentation    = doc,
          scopeStartLine   = ScopeColumn.EMPTY,
          scopeStartOffset = ScopeColumn.EMPTY,
          scopeEndLine     = ScopeColumn.EMPTY,
          scopeEndOffset   = ScopeColumn.EMPTY
        )
        row -> args
      case Suggestion.Method(
            expr,
            module,
            name,
            args,
            selfType,
            returnType,
            doc
          ) =>
        val row = SuggestionRow(
          id               = None,
          externalIdLeast  = expr.map(_.getLeastSignificantBits),
          externalIdMost   = expr.map(_.getMostSignificantBits),
          kind             = SuggestionKind.METHOD,
          module           = module,
          name             = name,
          selfType         = selfType,
          returnType       = returnType,
          documentation    = doc,
          scopeStartLine   = ScopeColumn.EMPTY,
          scopeStartOffset = ScopeColumn.EMPTY,
          scopeEndLine     = ScopeColumn.EMPTY,
          scopeEndOffset   = ScopeColumn.EMPTY
        )
        row -> args
      case Suggestion.Function(expr, module, name, args, returnType, scope) =>
        val row = SuggestionRow(
          id               = None,
          externalIdLeast  = expr.map(_.getLeastSignificantBits),
          externalIdMost   = expr.map(_.getMostSignificantBits),
          kind             = SuggestionKind.FUNCTION,
          module           = module,
          name             = name,
          selfType         = SelfTypeColumn.EMPTY,
          returnType       = returnType,
          documentation    = None,
          scopeStartLine   = scope.start.line,
          scopeStartOffset = scope.start.character,
          scopeEndLine     = scope.end.line,
          scopeEndOffset   = scope.end.character
        )
        row -> args
      case Suggestion.Local(expr, module, name, returnType, scope) =>
        val row = SuggestionRow(
          id               = None,
          externalIdLeast  = expr.map(_.getLeastSignificantBits),
          externalIdMost   = expr.map(_.getMostSignificantBits),
          kind             = SuggestionKind.LOCAL,
          module           = module,
          name             = name,
          selfType         = SelfTypeColumn.EMPTY,
          returnType       = returnType,
          documentation    = None,
          scopeStartLine   = scope.start.line,
          scopeStartOffset = scope.start.character,
          scopeEndLine     = scope.end.line,
          scopeEndOffset   = scope.end.character
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
          externalId =
            toUUID(suggestion.externalIdLeast, suggestion.externalIdMost),
          module        = suggestion.module,
          name          = suggestion.name,
          arguments     = arguments.sortBy(_.index).map(toArgument),
          returnType    = suggestion.returnType,
          documentation = suggestion.documentation
        )
      case SuggestionKind.METHOD =>
        Suggestion.Method(
          externalId =
            toUUID(suggestion.externalIdLeast, suggestion.externalIdMost),
          module        = suggestion.module,
          name          = suggestion.name,
          arguments     = arguments.sortBy(_.index).map(toArgument),
          selfType      = suggestion.selfType,
          returnType    = suggestion.returnType,
          documentation = suggestion.documentation
        )
      case SuggestionKind.FUNCTION =>
        Suggestion.Function(
          externalId =
            toUUID(suggestion.externalIdLeast, suggestion.externalIdMost),
          module     = suggestion.module,
          name       = suggestion.name,
          arguments  = arguments.sortBy(_.index).map(toArgument),
          returnType = suggestion.returnType,
          scope = Suggestion.Scope(
            Suggestion.Position(
              suggestion.scopeStartLine,
              suggestion.scopeStartOffset
            ),
            Suggestion.Position(
              suggestion.scopeEndLine,
              suggestion.scopeEndOffset
            )
          )
        )
      case SuggestionKind.LOCAL =>
        Suggestion.Local(
          externalId =
            toUUID(suggestion.externalIdLeast, suggestion.externalIdMost),
          module     = suggestion.module,
          name       = suggestion.name,
          returnType = suggestion.returnType,
          scope = Suggestion.Scope(
            Suggestion.Position(
              suggestion.scopeStartLine,
              suggestion.scopeStartOffset
            ),
            Suggestion.Position(
              suggestion.scopeEndLine,
              suggestion.scopeEndOffset
            )
          )
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

  /** Convert bits to the UUID.
    *
    * @param least the least significant bits of the UUID
    * @param most the most significant bits of the UUID
    * @return the new UUID
    */
  private def toUUID(least: Option[Long], most: Option[Long]): Option[UUID] =
    for {
      l <- least
      m <- most
    } yield new UUID(m, l)
}

object SqlSuggestionsRepo {

  /** Create the suggestions repo.
    *
    * @return the suggestions repo backed up by SQL database.
    */
  def apply()(implicit ec: ExecutionContext): SqlSuggestionsRepo = {
    new SqlSuggestionsRepo(new SqlDatabase())
  }

  /** Create the suggestions repo.
    *
    * @param path the path to the database file.
    * @return the suggestions repo backed up by SQL database.
    */
  def apply(path: File)(implicit ec: ExecutionContext): SqlSuggestionsRepo = {
    new SqlSuggestionsRepo(SqlDatabase(path.toString))
  }
}
