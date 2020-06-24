package org.enso.searcher.sql

import org.enso.searcher.Suggestion
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.ExecutionContext

/** The object for accessing the suggestions database. */
final class SqlSuggestionsRepo(implicit ec: ExecutionContext)
    extends SuggestionsRepo[DBIO] {

  /** The query returning the arguments joined with the corresponding
    * suggestions. */
  private val joined: Query[
    (Rep[Option[ArgumentsTable]], SuggestionsTable),
    (Option[ArgumentRow], SuggestionRow),
    Seq
  ] =
    arguments
      .joinRight(suggestions)
      .on(_.suggestionId === _.id)

  /** @inheritdoc **/
  override def findBy(returnType: String): DBIO[Seq[Suggestion]] = {
    val query = for {
      (argument, suggestion) <- joined
      if suggestion.returnType === returnType
    } yield (argument, suggestion)
    query.result.map(joinedToSuggestion)
  }

  /** @inheritdoc **/
  override def select(id: Long): DBIO[Option[Suggestion]] = {
    val query = for {
      (argument, suggestion) <- joined
      if suggestion.id === id
    } yield (argument, suggestion)
    query.result.map(coll => joinedToSuggestion(coll).headOption)
  }

  /** @inheritdoc **/
  override def insert(suggestion: Suggestion): DBIO[Long] = {
    val (suggestionRow, args) = toSuggestionRow(suggestion)
    for {
      id <- suggestions.returning(suggestions.map(_.id)) += suggestionRow
      _  <- arguments ++= args.map(toArgumentRow(id, _))
    } yield id
  }

  private def joinedToSuggestion(
    coll: Seq[(Option[ArgumentRow], SuggestionRow)]
  ): Seq[Suggestion] = {
    coll
      .groupBy(_._2)
      .view
      .mapValues(_.flatMap(_._1))
      .map(Function.tupled(toSuggestion))
      .toSeq
  }

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
          scopeStart    = None,
          scopeEnd      = None
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
          scopeStart    = None,
          scopeEnd      = None
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
          scopeStart    = Some(scope.start),
          scopeEnd      = Some(scope.end)
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
          scopeStart    = Some(scope.start),
          scopeEnd      = Some(scope.end)
        )
        row -> Seq()
    }

  private def toArgumentRow(
    suggestionId: Long,
    argument: Suggestion.Argument
  ): ArgumentRow =
    ArgumentRow(
      id           = None,
      suggestionId = suggestionId,
      name         = argument.name,
      tpe          = argument.reprType,
      isSuspended  = argument.isSuspended,
      hasDefault   = argument.hasDefault,
      defaultValue = argument.defaultValue
    )

  private def toSuggestion(
    suggestion: SuggestionRow,
    arguments: Seq[ArgumentRow]
  ): Suggestion =
    suggestion.kind match {
      case SuggestionKind.ATOM =>
        Suggestion.Atom(
          name          = suggestion.name,
          arguments     = arguments.map(toArgument),
          returnType    = suggestion.returnType,
          documentation = suggestion.documentation
        )
      case SuggestionKind.METHOD =>
        Suggestion.Method(
          name          = suggestion.name,
          arguments     = arguments.map(toArgument),
          selfType      = suggestion.selfType.get,
          returnType    = suggestion.returnType,
          documentation = suggestion.documentation
        )
      case SuggestionKind.FUNCTION =>
        Suggestion.Function(
          name       = suggestion.name,
          arguments  = arguments.map(toArgument),
          returnType = suggestion.returnType,
          scope =
            Suggestion.Scope(suggestion.scopeStart.get, suggestion.scopeEnd.get)
        )
      case SuggestionKind.LOCAL =>
        Suggestion.Local(
          name       = suggestion.name,
          returnType = suggestion.returnType,
          scope =
            Suggestion.Scope(suggestion.scopeStart.get, suggestion.scopeEnd.get)
        )

      case k =>
        throw new NoSuchElementException(s"Unknown suggestion kind: $k")
    }

  private def toArgument(row: ArgumentRow): Suggestion.Argument =
    Suggestion.Argument(
      name         = row.name,
      reprType     = row.tpe,
      isSuspended  = row.isSuspended,
      hasDefault   = row.hasDefault,
      defaultValue = row.defaultValue
    )
}
