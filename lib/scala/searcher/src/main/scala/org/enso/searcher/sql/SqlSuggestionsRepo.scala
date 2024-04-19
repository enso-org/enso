package org.enso.searcher.sql

import org.enso.polyglot.runtime.Runtime.Api._
import org.enso.polyglot.Suggestion
import org.enso.searcher.data.QueryResult
import org.enso.searcher.{SuggestionEntry, SuggestionsRepo}
import slick.jdbc.SQLiteProfile.api._
import slick.jdbc.meta.MTable
import slick.relational.RelationalProfile

import java.util.UUID

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** The object for accessing the suggestions database. */
final class SqlSuggestionsRepo(val db: SqlDatabase)(implicit
  ec: ExecutionContext
) extends SuggestionsRepo[Future] {

  /** Initialize the repo. */
  override def init: Future[Unit] =
    db.run(initQuery)

  /** @inheritdoc */
  override def clean: Future[Unit] =
    db.run(cleanQuery)

  /** @inheritdoc */
  override def getAll: Future[(Long, Seq[SuggestionEntry])] =
    db.run(getAllQuery)

  /** @inheritdoc */
  override def select(id: Long): Future[Option[Suggestion]] =
    db.run(selectQuery(id))

  /** @inheritdoc */
  override def insert(suggestion: Suggestion): Future[Option[Long]] =
    db.run(insertQuery(suggestion))

  /** @inheritdoc */
  override def insertAll(
    suggestions: Seq[Suggestion]
  ): Future[(Long, Seq[Long])] =
    db.run(insertAllWithVersionQuery(suggestions).transactionally)

  /** @inheritdoc */
  override def applyTree(
    tree: Seq[SuggestionUpdate]
  ): Future[Seq[QueryResult[SuggestionUpdate]]] =
    db.run(applyTreeQuery(tree).transactionally)

  /** @inheritdoc */
  override def applyActions(
    actions: Seq[SuggestionsDatabaseAction]
  ): Future[Seq[QueryResult[SuggestionsDatabaseAction]]] =
    db.run(applyActionsQuery(actions).transactionally)

  /** @inheritdoc */
  def getExportedSymbols(
    actions: Seq[ExportsUpdate]
  ): Future[Seq[QueryResult[ExportsUpdate]]] =
    db.run(getExportedSymbolsQuery(actions).transactionally)

  /** @inheritdoc */
  override def remove(suggestion: Suggestion): Future[Option[Long]] =
    db.run(removeQuery(suggestion))

  /** @inheritdoc */
  override def removeModules(modules: Seq[String]): Future[(Long, Seq[Long])] =
    db.run(removeByModuleQuery(modules))

  /** @inheritdoc */
  override def update(
    suggestion: Suggestion,
    externalId: Option[Option[Suggestion.ExternalID]],
    returnType: Option[String],
    documentation: Option[Option[String]],
    scope: Option[Suggestion.Scope]
  ): Future[(Long, Option[Long])] =
    db.run(
      updateQuery(
        suggestion,
        externalId,
        returnType,
        documentation,
        scope
      )
    )

  /** @inheritdoc */
  override def updateAll(
    expressions: Seq[(Suggestion.ExternalID, String)]
  ): Future[(Long, Seq[Option[Long]])] =
    db.run(updateAllQuery(expressions).transactionally)

  /** @inheritdoc */
  override def currentVersion: Future[Long] =
    db.run(currentVersionQuery)

  /** Close the database. */
  def close(): Unit =
    db.close()

  def insertBatchJava(suggestions: Array[Suggestion]): Future[Int] =
    db.run(insertBatchJavaQuery(suggestions).transactionally)

  def selectAllSuggestions: Future[Seq[SuggestionEntry]] =
    db.run(selectAllSuggestionsQuery.transactionally)

  /** The query to initialize the repo. */
  private def initQuery: DBIO[Unit] = {
    type RelationalTable[A] = RelationalProfile#Table[A]
    def checkVersion(version: Long) =
      if (version == SchemaVersion.CurrentVersion) {
        DBIO.successful(())
      } else {
        DBIO.failed(new InvalidSchemaVersion(version))
      }
    def createSchema(table: TableQuery[RelationalTable[_]]) =
      for {
        tables <- MTable.getTables(table.shaped.value.tableName)
        _      <- if (tables.isEmpty) table.schema.create else DBIO.successful(())
      } yield ()

    val tables: Seq[TableQuery[RelationalTable[_]]] =
      Seq(Suggestions, SuggestionsVersion, SchemaVersion)
        .asInstanceOf[Seq[TableQuery[RelationalTable[_]]]]
    val initSchemas =
      for {
        _       <- DBIO.sequence(tables.map(createSchema))
        version <- initSchemaVersionQuery
      } yield version

    for {
      versionAttempt <- currentSchemaVersionQuery.asTry
      version        <- versionAttempt.fold(_ => initSchemas, DBIO.successful)
      _              <- checkVersion(version)
    } yield ()
  }

  /** The query to clean the repo. */
  private def cleanQuery: DBIO[Unit] = {
    DBIO.seq(Suggestions.delete, SuggestionsVersion.delete)
  }

  /** The query to get all suggestions.
    *
    * @return the current database version with the list of suggestion entries
    */
  private def getAllQuery: DBIO[(Long, Seq[SuggestionEntry])] = {
    for {
      rows    <- Suggestions.result
      version <- currentVersionQuery
    } yield (version, rows.map(toSuggestionEntry))
  }

  /** The query to select the suggestion by id.
    *
    * @param id the id of a suggestion
    * @return return the suggestion
    */
  private def selectQuery(id: Long): DBIO[Option[Suggestion]] = {
    for {
      rows <- Suggestions.filter(_.id === id).result
    } yield rows.headOption.map(toSuggestion)
  }

  /** The query to insert the suggestion
    *
    * @param suggestion the suggestion to insert
    * @return the id of an inserted suggestion
    */
  private def insertQuery(suggestion: Suggestion): DBIO[Option[Long]] = {
    val suggestionRow = toSuggestionRow(suggestion)
    val query = for {
      id <- Suggestions.returning(Suggestions.map(_.id)) += suggestionRow
      _  <- incrementVersionQuery
    } yield id
    query.asTry.map {
      case Failure(_)  => None
      case Success(id) => Some(id)
    }
  }

  /** The query to apply the suggestion updates.
    *
    * @param tree the sequence of updates
    * @return the result of applying updates with the new database version
    */
  private def applyTreeQuery(
    tree: Seq[SuggestionUpdate]
  ): DBIO[Seq[QueryResult[SuggestionUpdate]]] = {
    val queries = tree.map {
      case update @ SuggestionUpdate(suggestion, action) =>
        val query = action match {
          case SuggestionAction.Add() =>
            insertQuery(suggestion)
          case SuggestionAction.Remove() =>
            removeQuery(suggestion)
          case SuggestionAction.Modify(
                extId,
                args,
                returnType,
                doc,
                scope,
                reexport
              ) =>
            if (
              extId.isDefined ||
              args.isDefined ||
              returnType.isDefined ||
              doc.isDefined ||
              scope.isDefined ||
              reexport.isDefined
            ) {
              updateSuggestionQuery(
                suggestion,
                extId,
                returnType,
                doc,
                scope
              )
            } else {
              DBIO.successful(None)
            }
        }
        query.map(rs => QueryResult(rs.toSeq, update))
    }
    DBIO.sequence(queries)
  }

  /** The query to apply the sequence of actions on the database.
    *
    * @param actions the list of actions
    * @return the result of applying actions
    */
  private def applyActionsQuery(
    actions: Seq[SuggestionsDatabaseAction]
  ): DBIO[Seq[QueryResult[SuggestionsDatabaseAction]]] = {
    val removeActions = actions.map {
      case act @ SuggestionsDatabaseAction.Clean(module) =>
        for {
          ids <- removeModulesQuery(Seq(module))
        } yield QueryResult[SuggestionsDatabaseAction](ids, act)
    }
    DBIO.sequence(removeActions)
  }

  /** The query to get the suggestions related to the export updates.
    *
    * @param actions the list of updates
    * @return the suggestions ids associated with the export updates
    */
  private def getExportedSymbolsQuery(
    actions: Seq[ExportsUpdate]
  ): DBIO[Seq[QueryResult[ExportsUpdate]]] = {
    val qs = actions.map { action =>
      val actionIdQueries = action.exports.symbols.toSeq.map { symbol =>
        selectExportedSymbolQuery(
          symbol.module,
          symbol.name,
          symbol.kind
        ).result
      }
      for {
        ids <- DBIO.sequence(actionIdQueries)
      } yield QueryResult(ids.flatten, action)
    }
    DBIO.sequence(qs)
  }

  /** The query to select the exported symbol.
    *
    * @param module the module name of the exported symbol
    * @param name the name of the exported symbol
    * @param kind the kind of the exported symbol
    * @return the database query returning the list of ids corresponding to the
    * exported symbol
    */
  private def selectExportedSymbolQuery(
    module: String,
    name: String,
    kind: Suggestion.Kind
  ): Query[Rep[Long], Long, Seq] =
    Suggestions
      .filter(_.module === module)
      .filter(_.kind === SuggestionKind(kind))
      .filter(_.name === name)
      .take(1)
      .map(_.id)

  /** The query to select the suggestion.
    *
    * @param raw the suggestion converted to the row form
    * @return the database query
    */
  private def selectSuggestionQuery(
    raw: SuggestionRow
  ): Query[SuggestionsTable, SuggestionRow, Seq] = {
    Suggestions
      .filter(_.module === raw.module)
      .filter(_.kind === raw.kind)
      .filter(_.name === raw.name)
      .filter(_.selfType === raw.selfType)
      .filter(_.scopeStartLine === raw.scopeStartLine)
      .filter(_.scopeStartOffset === raw.scopeStartOffset)
      .filter(_.scopeEndLine === raw.scopeEndLine)
      .filter(_.scopeEndOffset === raw.scopeEndOffset)
  }

  /** The query to remove the suggestion.
    *
    * @param suggestion the suggestion to remove
    * @return the id of removed suggestion
    */
  private def removeQuery(suggestion: Suggestion): DBIO[Option[Long]] = {
    val raw         = toSuggestionRow(suggestion)
    val selectQuery = selectSuggestionQuery(raw)
    val deleteQuery = for {
      rows <- selectQuery.result
      n    <- selectQuery.delete
      _    <- if (n > 0) incrementVersionQuery else DBIO.successful(())
    } yield rows.flatMap(_.id).headOption
    deleteQuery
  }

  /** The query to remove the suggestions by module name
    *
    * @param modules the module names to remove
    * @return the current database version and a list of removed suggestion ids
    */
  private def removeByModuleQuery(
    modules: Seq[String]
  ): DBIO[(Long, Seq[Long])] = {
    val deleteQuery = for {
      ids <- removeModulesQuery(modules)
      version <-
        if (ids.nonEmpty) incrementVersionQuery else currentVersionQuery
    } yield version -> ids
    deleteQuery
  }

  /** The query to remove the suggestions by module name
    *
    * @param modules the module names to remove
    * @return the list of removed suggestion ids
    */
  private def removeModulesQuery(
    modules: Seq[String]
  ): DBIO[Seq[Long]] = {
    val selectQuery = Suggestions.filter(_.module.inSet(modules))
    for {
      rows <- selectQuery.map(_.id).result
      _    <- selectQuery.delete
    } yield rows
  }

  /** The query to update a suggestion.
    *
    * @param externalId the external id of a suggestion
    * @param returnType the new return type
    * @return the id of updated suggestion
    */
  private def updateByExternalIdQuery(
    externalId: Suggestion.ExternalID,
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

  /** The query to update the suggestion.
    *
    * @param suggestion the key suggestion
    * @param externalId the external id to update
    * @param returnType the return type to update
    * @param documentation the documentation string to update
    * @param scope the scope to update
    */
  private def updateQuery(
    suggestion: Suggestion,
    externalId: Option[Option[Suggestion.ExternalID]],
    returnType: Option[String],
    documentation: Option[Option[String]],
    scope: Option[Suggestion.Scope]
  ): DBIO[(Long, Option[Long])] =
    for {
      idOpt <- updateSuggestionQuery(
        suggestion,
        externalId,
        returnType,
        documentation,
        scope
      )
      version <- currentVersionQuery
    } yield (version, idOpt)

  /** The query to update the suggestion.
    *
    * @param suggestion the key suggestion
    * @param externalId the external id to update
    * @param returnType the return type to update
    * @param documentation the documentation string to update
    * @param scope the scope to update
    */
  private def updateSuggestionQuery(
    suggestion: Suggestion,
    externalId: Option[Option[Suggestion.ExternalID]],
    returnType: Option[String],
    documentation: Option[Option[String]],
    scope: Option[Suggestion.Scope]
  ): DBIO[Option[Long]] = {
    val raw   = toSuggestionRow(suggestion)
    val query = selectSuggestionQuery(raw)

    val updateQ = for {
      r1 <- DBIO.sequenceOption {
        externalId.map { extIdOpt =>
          query
            .map(r => (r.externalIdLeast, r.externalIdMost))
            .update(
              (
                extIdOpt.map(_.getLeastSignificantBits),
                extIdOpt.map(_.getMostSignificantBits)
              )
            )
        }
      }
      r2 <- DBIO.sequenceOption {
        returnType.map(tpe => query.map(_.returnType).update(tpe))
      }
      r3 <- DBIO.sequenceOption {
        documentation.map(doc => query.map(_.documentation).update(doc))
      }
      r4 <- DBIO.sequenceOption {
        scope.map { s =>
          query
            .map(r =>
              (
                r.scopeStartLine,
                r.scopeStartOffset,
                r.scopeEndLine,
                r.scopeEndOffset
              )
            )
            .update(
              (s.start.line, s.start.character, s.end.line, s.end.character)
            )
        }
      }
    } yield (r1 ++ r2 ++ r3 ++ r4).sum
    for {
      id <- query.map(_.id).result.headOption
      n  <- updateQ
      _  <- if (n > 0) incrementVersionQuery else DBIO.successful(())
    } yield id
  }

  /** The query to update a list of suggestions by external id.
    *
    * @param expressions the list of expressions to update
    * @return the current database version with the list of updated suggestion ids
    */
  private def updateAllQuery(
    expressions: Seq[(Suggestion.ExternalID, String)]
  ): DBIO[(Long, Seq[Option[Long]])] = {
    val query = for {
      ids <- DBIO.sequence(
        expressions.map(Function.tupled(updateByExternalIdQuery))
      )
      version <-
        if (ids.exists(_.nonEmpty)) incrementVersionQuery
        else currentVersionQuery
    } yield (version, ids)
    query
  }

  /** The query to get current version of the repo. */
  private def currentVersionQuery: DBIO[Long] = {
    for {
      versionOpt <- SuggestionsVersion.result.headOption
    } yield versionOpt.flatMap(_.id).getOrElse(0L)
  }

  /** The query to increment the current version of the repo. */
  private def incrementVersionQuery: DBIO[Long] = {
    for {
      version <- SuggestionsVersion.returning(
        SuggestionsVersion.map(_.id)
      ) += SuggestionsVersionRow(None)
      _ <- SuggestionsVersion.filterNot(_.id === version).delete
    } yield version
  }

  /** The query to get current version of the repo. */
  private def currentSchemaVersionQuery: DBIO[Long] = {
    for {
      versionOpt <- SchemaVersion.result.headOption
    } yield versionOpt.flatMap(_.id).getOrElse(0L)
  }

  /** The query to initialize the [[SchemaVersion]] table. */
  private def initSchemaVersionQuery: DBIO[Long] = {
    setSchemaVersionQuery(SchemaVersion.CurrentVersion)
  }

  /** The query setting the schema version.
    *
    * @param version the schema version.
    * @return the current value of the schema version
    */
  private def setSchemaVersionQuery(version: Long): DBIO[Long] = {
    val query = for {
      _ <- SchemaVersion.delete
      _ <- SchemaVersion += SchemaVersionRow(Some(version))
    } yield version
    query
  }

  /** The query to insert suggestions in a batch.
    *
    * @param suggestions the list of suggestions to insert
    * @return the current size of the database
    */
  private def insertBatchJavaQuery(
    suggestions: Iterable[Suggestion]
  ): DBIO[Int] = {
    val rows = suggestions.map(toSuggestionRow)
    for {
      _    <- (Suggestions ++= rows).asTry
      size <- Suggestions.length.result
    } yield size
  }

  /** The query to insert suggestions in a batch.
    *
    * @param suggestions the list of suggestions to insert
    * @return the current size of the database
    */
  private def insertAllQuery(
    suggestions: Iterable[Suggestion]
  ): DBIO[Seq[Long]] = {
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
      val rows = suggestions.map(toSuggestionRow)
      for {
        _    <- Suggestions ++= rows
        rows <- Suggestions.result
      } yield {
        val rowsMap =
          rows.map(r => SuggestionRowUniqueIndex(r) -> r.id.get).toMap
        suggestionsMap.keys.map(rowsMap(_)).toSeq
      }
    } else {
      DBIO.failed(SqlSuggestionsRepo.UniqueConstraintViolatedError(duplicates))
    }
  }

  private def insertAllWithVersionQuery(
    suggestions: Iterable[Suggestion]
  ): DBIO[(Long, Seq[Long])] = {
    for {
      ids     <- insertAllQuery(suggestions)
      version <- incrementVersionQuery
    } yield (version, ids)
  }

  private def selectAllSuggestionsQuery: DBIO[Seq[SuggestionEntry]] =
    for {
      rows <- Suggestions.result
    } yield {
      rows.map(row => SuggestionEntry(row.id.get, toSuggestion(row)))
    }

  /** Convert the suggestion to a row in the suggestions table. */
  private def toSuggestionRow(suggestion: Suggestion): SuggestionRow =
    suggestion match {
      case Suggestion.Module(module, doc, _) =>
        SuggestionRow(
          id               = None,
          externalIdLeast  = None,
          externalIdMost   = None,
          kind             = SuggestionKind.MODULE,
          module           = module,
          name             = module,
          selfType         = SelfTypeColumn.EMPTY,
          returnType       = "",
          parentType       = None,
          isStatic         = false,
          scopeStartLine   = ScopeColumn.EMPTY,
          scopeStartOffset = ScopeColumn.EMPTY,
          scopeEndLine     = ScopeColumn.EMPTY,
          scopeEndOffset   = ScopeColumn.EMPTY,
          documentation    = doc
        )
      case Suggestion.Type(
            expr,
            module,
            name,
            _,
            returnType,
            parentType,
            doc,
            _
          ) =>
        SuggestionRow(
          id               = None,
          externalIdLeast  = expr.map(_.getLeastSignificantBits),
          externalIdMost   = expr.map(_.getMostSignificantBits),
          kind             = SuggestionKind.TYPE,
          module           = module,
          name             = name,
          selfType         = SelfTypeColumn.EMPTY,
          returnType       = returnType,
          parentType       = parentType,
          isStatic         = false,
          documentation    = doc,
          scopeStartLine   = ScopeColumn.EMPTY,
          scopeStartOffset = ScopeColumn.EMPTY,
          scopeEndLine     = ScopeColumn.EMPTY,
          scopeEndOffset   = ScopeColumn.EMPTY
        )
      case Suggestion.Constructor(
            expr,
            module,
            name,
            _,
            returnType,
            doc,
            _,
            _
          ) =>
        SuggestionRow(
          id               = None,
          externalIdLeast  = expr.map(_.getLeastSignificantBits),
          externalIdMost   = expr.map(_.getMostSignificantBits),
          kind             = SuggestionKind.CONSTRUCTOR,
          module           = module,
          name             = name,
          selfType         = returnType,
          returnType       = returnType,
          parentType       = None,
          isStatic         = false,
          documentation    = doc,
          scopeStartLine   = ScopeColumn.EMPTY,
          scopeStartOffset = ScopeColumn.EMPTY,
          scopeEndLine     = ScopeColumn.EMPTY,
          scopeEndOffset   = ScopeColumn.EMPTY
        )
      case Suggestion.Getter(
            expr,
            module,
            name,
            _,
            selfType,
            returnType,
            doc,
            _,
            _
          ) =>
        SuggestionRow(
          id               = None,
          externalIdLeast  = expr.map(_.getLeastSignificantBits),
          externalIdMost   = expr.map(_.getMostSignificantBits),
          kind             = SuggestionKind.GETTER,
          module           = module,
          name             = name,
          selfType         = selfType,
          returnType       = returnType,
          parentType       = None,
          isStatic         = false,
          documentation    = doc,
          scopeStartLine   = ScopeColumn.EMPTY,
          scopeStartOffset = ScopeColumn.EMPTY,
          scopeEndLine     = ScopeColumn.EMPTY,
          scopeEndOffset   = ScopeColumn.EMPTY
        )
      case Suggestion.DefinedMethod(
            expr,
            module,
            name,
            _,
            selfType,
            returnType,
            isStatic,
            doc,
            _,
            _
          ) =>
        SuggestionRow(
          id               = None,
          externalIdLeast  = expr.map(_.getLeastSignificantBits),
          externalIdMost   = expr.map(_.getMostSignificantBits),
          kind             = SuggestionKind.METHOD,
          module           = module,
          name             = name,
          selfType         = selfType,
          returnType       = returnType,
          parentType       = None,
          isStatic         = isStatic,
          documentation    = doc,
          scopeStartLine   = ScopeColumn.EMPTY,
          scopeStartOffset = ScopeColumn.EMPTY,
          scopeEndLine     = ScopeColumn.EMPTY,
          scopeEndOffset   = ScopeColumn.EMPTY
        )
      case Suggestion.Conversion(
            expr,
            module,
            _,
            sourceType,
            returnType,
            doc,
            _
          ) =>
        SuggestionRow(
          id               = None,
          externalIdLeast  = expr.map(_.getLeastSignificantBits),
          externalIdMost   = expr.map(_.getMostSignificantBits),
          kind             = SuggestionKind.CONVERSION,
          module           = module,
          name             = NameColumn.conversionMethodName(sourceType, returnType),
          selfType         = sourceType,
          returnType       = returnType,
          parentType       = None,
          isStatic         = false,
          documentation    = doc,
          scopeStartLine   = ScopeColumn.EMPTY,
          scopeStartOffset = ScopeColumn.EMPTY,
          scopeEndLine     = ScopeColumn.EMPTY,
          scopeEndOffset   = ScopeColumn.EMPTY
        )
      case Suggestion.Function(
            expr,
            module,
            name,
            _,
            returnType,
            scope,
            doc
          ) =>
        SuggestionRow(
          id               = None,
          externalIdLeast  = expr.map(_.getLeastSignificantBits),
          externalIdMost   = expr.map(_.getMostSignificantBits),
          kind             = SuggestionKind.FUNCTION,
          module           = module,
          name             = name,
          selfType         = SelfTypeColumn.EMPTY,
          returnType       = returnType,
          parentType       = None,
          isStatic         = false,
          documentation    = doc,
          scopeStartLine   = scope.start.line,
          scopeStartOffset = scope.start.character,
          scopeEndLine     = scope.end.line,
          scopeEndOffset   = scope.end.character
        )
      case Suggestion.Local(expr, module, name, returnType, scope, doc) =>
        SuggestionRow(
          id               = None,
          externalIdLeast  = expr.map(_.getLeastSignificantBits),
          externalIdMost   = expr.map(_.getMostSignificantBits),
          kind             = SuggestionKind.LOCAL,
          module           = module,
          name             = name,
          selfType         = SelfTypeColumn.EMPTY,
          returnType       = returnType,
          parentType       = None,
          isStatic         = false,
          documentation    = doc,
          scopeStartLine   = scope.start.line,
          scopeStartOffset = scope.start.character,
          scopeEndLine     = scope.end.line,
          scopeEndOffset   = scope.end.character
        )
    }

  /** Convert the database rows to a suggestion entry. */
  private def toSuggestionEntry(suggestion: SuggestionRow): SuggestionEntry =
    SuggestionEntry(suggestion.id.get, toSuggestion(suggestion))

  /** Convert the database rows to a suggestion. */
  private def toSuggestion(suggestion: SuggestionRow): Suggestion =
    suggestion.kind match {
      case SuggestionKind.MODULE =>
        Suggestion.Module(
          module        = suggestion.module,
          documentation = suggestion.documentation
        )
      case SuggestionKind.TYPE =>
        Suggestion.Type(
          externalId =
            toUUID(suggestion.externalIdLeast, suggestion.externalIdMost),
          module        = suggestion.module,
          name          = suggestion.name,
          params        = Seq(),
          returnType    = suggestion.returnType,
          parentType    = suggestion.parentType,
          documentation = suggestion.documentation
        )
      case SuggestionKind.CONSTRUCTOR =>
        Suggestion.Constructor(
          externalId =
            toUUID(suggestion.externalIdLeast, suggestion.externalIdMost),
          module        = suggestion.module,
          name          = suggestion.name,
          arguments     = Seq(),
          returnType    = suggestion.returnType,
          documentation = suggestion.documentation,
          annotations   = Seq()
        )
      case SuggestionKind.GETTER =>
        Suggestion.Getter(
          externalId =
            toUUID(suggestion.externalIdLeast, suggestion.externalIdMost),
          module        = suggestion.module,
          name          = suggestion.name,
          arguments     = Seq(),
          selfType      = suggestion.selfType,
          returnType    = suggestion.returnType,
          documentation = suggestion.documentation,
          annotations   = Seq()
        )
      case SuggestionKind.METHOD =>
        Suggestion.DefinedMethod(
          externalId =
            toUUID(suggestion.externalIdLeast, suggestion.externalIdMost),
          module        = suggestion.module,
          name          = suggestion.name,
          arguments     = Seq(),
          selfType      = suggestion.selfType,
          returnType    = suggestion.returnType,
          isStatic      = suggestion.isStatic,
          documentation = suggestion.documentation,
          annotations   = Seq()
        )
      case SuggestionKind.CONVERSION =>
        Suggestion.Conversion(
          externalId =
            toUUID(suggestion.externalIdLeast, suggestion.externalIdMost),
          module        = suggestion.module,
          arguments     = Seq(),
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
          arguments  = Seq(),
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
          ),
          documentation = suggestion.documentation
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
          ),
          documentation = suggestion.documentation
        )
      case k =>
        throw new NoSuchElementException(s"Unknown suggestion kind: $k")
    }

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

  /** An error indicating that the database unique constraint was violated.
    *
    * @param duplicates the entries that violate the unique constraint
    */
  final case class UniqueConstraintViolatedError(
    duplicates: Seq[(Suggestion, Suggestion)]
  ) extends Exception(s"Database unique constraint is violated [$duplicates].")
}
