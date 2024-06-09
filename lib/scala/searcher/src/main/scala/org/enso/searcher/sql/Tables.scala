package org.enso.searcher.sql

import org.enso.polyglot.Suggestion

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
  documentation: Option[String]
)

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
