package org.enso.searcher.sql

import org.enso.polyglot.Suggestion
import slick.jdbc.SQLiteProfile.api._

import scala.annotation.nowarn

/** A row in the suggestions table.
  *
  * @param id the id of a suggestion
  * @param externalIdLeast the least significant bits of external id
  * @param externalIdMost the most significant bits of external id
  * @param kind the type of a suggestion
  * @param module the module name
  * @param name the suggestion name
  * @param selfType the self type of a suggestion
  * @param returnType the return type of a suggestion
  * @param parentType qualified name of the parent type
  * @param isStatic the flag indicating whether a method is static or instance
  * @param scopeStartLine the line of the start position of the scope
  * @param scopeStartOffset the offset of the start position of the scope
  * @param scopeEndLine the line of the end position of the scope
  * @param scopeEndOffset the offset of the end position of the scope
  * @param documentation the documentation string
  */
case class SuggestionRow(
  id: Option[Long],
  externalIdLeast: Option[Long],
  externalIdMost: Option[Long],
  kind: Byte,
  module: String,
  name: String,
  selfType: String,
  returnType: String,
  parentType: Option[String],
  isStatic: Boolean,
  scopeStartLine: Int,
  scopeStartOffset: Int,
  scopeEndLine: Int,
  scopeEndOffset: Int,
  documentation: Option[String],
  reexport: Option[String]
)

/** A row in the suggestions_version table.
  *
  * @param id the row id
  */
case class SuggestionsVersionRow(id: Option[Long])

/** A row in the schema_version table.
  *
  * @param id the row id
  */
case class SchemaVersionRow(id: Option[Long])

/** The type of a suggestion. */
object SuggestionKind {

  val MODULE: Byte      = 0
  val TYPE: Byte        = 1
  val CONSTRUCTOR: Byte = 2
  val GETTER: Byte      = 3
  val METHOD: Byte      = 4
  val FUNCTION: Byte    = 5
  val LOCAL: Byte       = 6
  val CONVERSION: Byte  = 7

  /** Create a database suggestion kind.
    *
    * @param kind the suggestion kind
    * @return the representation of the suggestion kind in the database
    */
  def apply(kind: Suggestion.Kind): Byte =
    kind match {
      case Suggestion.Kind.Module      => MODULE
      case Suggestion.Kind.Type        => TYPE
      case Suggestion.Kind.Constructor => CONSTRUCTOR
      case Suggestion.Kind.Getter      => GETTER
      case Suggestion.Kind.Method      => METHOD
      case Suggestion.Kind.Conversion  => CONVERSION
      case Suggestion.Kind.Function    => FUNCTION
      case Suggestion.Kind.Local       => LOCAL
    }

  def toSuggestion(kind: Byte): Suggestion.Kind =
    kind match {
      case MODULE      => Suggestion.Kind.Module
      case TYPE        => Suggestion.Kind.Type
      case CONSTRUCTOR => Suggestion.Kind.Constructor
      case GETTER      => Suggestion.Kind.Getter
      case METHOD      => Suggestion.Kind.Method
      case FUNCTION    => Suggestion.Kind.Function
      case LOCAL       => Suggestion.Kind.Local
      case CONVERSION  => Suggestion.Kind.Conversion
    }
}

object ScopeColumn {

  /** A constant representing an empty value in the scope column. */
  val EMPTY: Int = -1
}

object SelfTypeColumn {

  /** A constant representing en empty value in the self type column. */
  val EMPTY: String = "\u0500"
}

object NameColumn {

  /** Create the method name for conversion */
  def conversionMethodName(
    sourceType: String,
    returnType: String
  ): String =
    s"${Suggestion.Kind.Conversion.From}_${sourceType}_${returnType}"

}

/** The schema of the suggestions table. */
@nowarn("msg=multiarg infix syntax")
final class SuggestionsTable(tag: Tag)
    extends Table[SuggestionRow](tag, "suggestions") {

  def id              = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def externalIdLeast = column[Option[Long]]("external_id_least")
  def externalIdMost  = column[Option[Long]]("external_id_most")
  def kind            = column[Byte]("kind")
  def module          = column[String]("module")
  def name            = column[String]("name")
  def selfType        = column[String]("self_type")
  def returnType      = column[String]("return_type")
  def parentType      = column[Option[String]]("parent_type")
  def isStatic        = column[Boolean]("is_static")
  def scopeStartLine =
    column[Int]("scope_start_line", O.Default(ScopeColumn.EMPTY))
  def scopeStartOffset =
    column[Int]("scope_start_offset", O.Default(ScopeColumn.EMPTY))
  def scopeEndLine =
    column[Int]("scope_end_line", O.Default(ScopeColumn.EMPTY))
  def scopeEndOffset =
    column[Int]("scope_end_offset", O.Default(ScopeColumn.EMPTY))
  def documentation = column[Option[String]]("documentation")
  def reexport      = column[Option[String]]("reexport")

  def * =
    (
      id.?,
      externalIdLeast,
      externalIdMost,
      kind,
      module,
      name,
      selfType,
      returnType,
      parentType,
      isStatic,
      scopeStartLine,
      scopeStartOffset,
      scopeEndLine,
      scopeEndOffset,
      documentation,
      reexport
    ) <>
    (SuggestionRow.tupled, SuggestionRow.unapply)

  def moduleIdx     = index("suggestions_module_idx", module)
  def name_idx      = index("suggestions_name_idx", name)
  def selfTypeIdx   = index("suggestions_self_type_idx", selfType)
  def returnTypeIdx = index("suggestions_return_type_idx", returnType)
  def externalIdIdx =
    index("suggestions_external_id_idx", (externalIdLeast, externalIdMost))
  def reexportIdx = index("suggestions_reexport_idx", reexport)
  // NOTE: unique index should not contain nullable columns because SQLite
  // teats NULLs as distinct values.
  def uniqueIdx =
    index(
      "suggestions_unique_idx",
      (
        kind,
        module,
        name,
        selfType,
        scopeStartLine,
        scopeStartOffset,
        scopeEndLine,
        scopeEndOffset
      ),
      unique = true
    )
}

/** An element of unique suggestion index.
  *
  * @param kind the type of a suggestion
  * @param module the module name
  * @param name the suggestion name
  * @param selfType the self type of a suggestion
  * @param scopeStartLine the line of the start position of the scope
  * @param scopeStartOffset the offset of the start position of the scope
  * @param scopeEndLine the line of the end position of the scope
  * @param scopeEndOffset the offset of the end position of the scope
  */
final case class SuggestionRowUniqueIndex(
  kind: Suggestion.Kind,
  module: String,
  name: String,
  selfType: String,
  scopeStartLine: Int,
  scopeStartOffset: Int,
  scopeEndLine: Int,
  scopeEndOffset: Int
)

object SuggestionRowUniqueIndex {

  /** Create an index element from the provided suggestion.
    *
    * @param suggestion the suggestion
    * @return an index element representing the provided suggestion
    */
  def apply(suggestion: Suggestion): SuggestionRowUniqueIndex = {
    val scope = Suggestion.Scope(suggestion)
    val suggestionName = suggestion match {
      case conversion: Suggestion.Conversion =>
        NameColumn.conversionMethodName(
          conversion.selfType,
          conversion.returnType
        )
      case _ => suggestion.name
    }
    new SuggestionRowUniqueIndex(
      Suggestion.Kind(suggestion),
      suggestion.module,
      suggestionName,
      Suggestion.SelfType(suggestion).getOrElse(SelfTypeColumn.EMPTY),
      scope.map(_.start.line).getOrElse(ScopeColumn.EMPTY),
      scope.map(_.start.character).getOrElse(ScopeColumn.EMPTY),
      scope.map(_.end.line).getOrElse(ScopeColumn.EMPTY),
      scope.map(_.end.character).getOrElse(ScopeColumn.EMPTY)
    )
  }

  /** Create an index element from the provided suggestion row.
    *
    * @param row the suggestion row
    * @return an index element representing the provided suggestion row
    */
  def apply(row: SuggestionRow): SuggestionRowUniqueIndex =
    new SuggestionRowUniqueIndex(
      SuggestionKind.toSuggestion(row.kind),
      row.module,
      row.name,
      row.selfType,
      row.scopeStartLine,
      row.scopeStartOffset,
      row.scopeEndLine,
      row.scopeEndOffset
    )
}

/** The schema of the suggestions_version table. */
@nowarn("msg=multiarg infix syntax")
final class SuggestionsVersionTable(tag: Tag)
    extends Table[SuggestionsVersionRow](tag, "suggestions_version") {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def * = id.? <> (SuggestionsVersionRow.apply, SuggestionsVersionRow.unapply)
}

/** The schema of the schema_version table. */
@nowarn("msg=multiarg infix syntax")
final class SchemaVersionTable(tag: Tag)
    extends Table[SchemaVersionRow](tag, "schema_version") {

  def id = column[Long]("id", O.PrimaryKey)

  def * = id.? <> (SchemaVersionRow.apply, SchemaVersionRow.unapply)
}

object Suggestions extends TableQuery(new SuggestionsTable(_))

object SuggestionsVersion extends TableQuery(new SuggestionsVersionTable(_))

object SchemaVersion extends TableQuery(new SchemaVersionTable(_)) {

  /** The current schema version. */
  val CurrentVersion: Long = 11
}
