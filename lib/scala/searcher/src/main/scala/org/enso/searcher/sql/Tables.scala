package org.enso.searcher.sql

import org.enso.searcher.Suggestion
import slick.jdbc.SQLiteProfile.api._

import scala.annotation.nowarn

/** A row in the arguments table.
  *
  * @param id the id of an argument
  * @param suggestionId the id of the suggestion
  * @param index the argument position in the arguments list
  * @param name the argument name
  * @param tpe the argument type
  * @param isSuspended is the argument lazy
  * @param hasDefault does the argument have the default value
  * @param defaultValue optional default value
  */
case class ArgumentRow(
  id: Option[Long],
  suggestionId: Long,
  index: Int,
  name: String,
  tpe: String,
  isSuspended: Boolean,
  hasDefault: Boolean,
  defaultValue: Option[String]
)

/** A row in the suggestions table.
  *
  * @param id the id of a suggestion
  * @param kind the type of a suggestion
  * @param name the suggestion name
  * @param selfType the self type of a suggestion
  * @param returnType the return type of a suggestion
  * @param documentation the documentation string
  * @param scopeStart the start of the scope
  * @param scopeEnd the end of the scope
  */
case class SuggestionRow(
  id: Option[Long],
  kind: Byte,
  name: String,
  selfType: String,
  returnType: String,
  documentation: Option[String],
  scopeStart: Int,
  scopeEnd: Int
)

/** A row in the versions table.
  *
  * @param id the row id
  */
case class VersionRow(id: Option[Long])

/** The type of a suggestion. */
object SuggestionKind {

  val ATOM: Byte     = 0
  val METHOD: Byte   = 1
  val FUNCTION: Byte = 2
  val LOCAL: Byte    = 3

  /** Create a database suggestion kind.
    *
    * @param kind the suggestion kind
    * @return the representation of the suggestion kind in the database
    */
  def apply(kind: Suggestion.Kind): Byte =
    kind match {
      case Suggestion.Kind.Atom     => ATOM
      case Suggestion.Kind.Method   => METHOD
      case Suggestion.Kind.Function => FUNCTION
      case Suggestion.Kind.Local    => LOCAL
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

/** The schema of the arguments table. */
@nowarn("msg=multiarg infix syntax")
final class ArgumentsTable(tag: Tag)
    extends Table[ArgumentRow](tag, "arguments") {

  def id           = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def suggestionId = column[Long]("suggestion_id")
  def index        = column[Int]("index")
  def name         = column[String]("name")
  def tpe          = column[String]("type")
  def isSuspended  = column[Boolean]("is_suspended", O.Default(false))
  def hasDefault   = column[Boolean]("has_default", O.Default(false))
  def defaultValue = column[Option[String]]("default_value")
  def * =
    (
      id.?,
      suggestionId,
      index,
      name,
      tpe,
      isSuspended,
      hasDefault,
      defaultValue
    ) <>
    (ArgumentRow.tupled, ArgumentRow.unapply)

  def suggestion =
    foreignKey("suggestion_fk", suggestionId, Suggestions)(
      _.id,
      onUpdate = ForeignKeyAction.Restrict,
      onDelete = ForeignKeyAction.Cascade
    )
}

/** The schema of the suggestions table. */
@nowarn("msg=multiarg infix syntax")
final class SuggestionsTable(tag: Tag)
    extends Table[SuggestionRow](tag, "suggestions") {

  def id            = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def kind          = column[Byte]("kind")
  def name          = column[String]("name")
  def selfType      = column[String]("self_type")
  def returnType    = column[String]("return_type")
  def documentation = column[Option[String]]("documentation")
  def scopeStart    = column[Int]("scope_start", O.Default(ScopeColumn.EMPTY))
  def scopeEnd      = column[Int]("scope_end", O.Default(ScopeColumn.EMPTY))
  def * =
    (
      id.?,
      kind,
      name,
      selfType,
      returnType,
      documentation,
      scopeStart,
      scopeEnd
    ) <>
    (SuggestionRow.tupled, SuggestionRow.unapply)

  def selfTypeIdx   = index("suggestions_self_type_idx", selfType)
  def returnTypeIdx = index("suggestions_return_type_idx", returnType)
  def name_idx      = index("suggestions_name_idx", name)
  // NOTE: unique index should not contain nullable columns because SQLite
  // teats NULLs as distinct values.
  def uniqueIdx =
    index(
      "suggestion_unique_idx",
      (kind, name, selfType, scopeStart, scopeEnd),
      unique = true
    )
}

/** The schema of the versions table. */
@nowarn("msg=multiarg infix syntax")
final class VersionsTable(tag: Tag) extends Table[VersionRow](tag, "version") {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def * = id.? <> (VersionRow.apply, VersionRow.unapply)
}

object Arguments extends TableQuery(new ArgumentsTable(_))

object Suggestions extends TableQuery(new SuggestionsTable(_))

object Versions extends TableQuery(new VersionsTable(_))
