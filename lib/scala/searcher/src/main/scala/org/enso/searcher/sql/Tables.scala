package org.enso.searcher.sql

import slick.jdbc.SQLiteProfile.api._

import scala.annotation.nowarn

/** A row in the arguments table.
  *
  * @param id the id of an argument
  * @param suggestionId the id of the suggestion
  * @param name the argument name
  * @param tpe the argument type
  * @param isSuspended is the argument lazy
  * @param hasDefault does the argument have the default value
  * @param defaultValue optional default value
  */
case class ArgumentRow(
  id: Option[Long],
  suggestionId: Long,
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
  selfType: Option[String],
  returnType: String,
  documentation: Option[String],
  scopeStart: Option[Int],
  scopeEnd: Option[Int]
)

/** The type of a suggestion. */
object SuggestionKind {

  val ATOM: Byte     = 0
  val METHOD: Byte   = 1
  val FUNCTION: Byte = 2
  val LOCAL: Byte    = 3
}

/** The schema of the arguments table. */
@nowarn("msg=multiarg infix syntax")
final class ArgumentsTable(tag: Tag)
    extends Table[ArgumentRow](tag, "arguments") {

  def id           = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def suggestionId = column[Long]("suggestion_id")
  def name         = column[String]("name")
  def tpe          = column[String]("type")
  def isSuspended  = column[Boolean]("is_suspended", O.Default(false))
  def hasDefault   = column[Boolean]("has_default", O.Default(false))
  def defaultValue = column[Option[String]]("default_value")
  def * =
    (id.?, suggestionId, name, tpe, isSuspended, hasDefault, defaultValue) <>
    (ArgumentRow.tupled, ArgumentRow.unapply)

  def suggestion =
    foreignKey("suggestion_fk", suggestionId, suggestions)(
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
  def selfType      = column[Option[String]]("self_type")
  def returnType    = column[String]("return_type")
  def documentation = column[Option[String]]("documentation")
  def scopeStart    = column[Option[Int]]("scope_start")
  def scopeEnd      = column[Option[Int]]("scope_end")
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

  def selfTypeIdx   = index("self_type_idx", selfType)
  def returnTypeIdx = index("return_type_idx", name)
}

object arguments extends TableQuery(new ArgumentsTable(_))

object suggestions extends TableQuery(new SuggestionsTable(_))
