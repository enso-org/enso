package org.enso.polyglot

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(
      value = classOf[ExportedSymbol.Module],
      name  = "exportedModule"
    ),
    new JsonSubTypes.Type(
      value = classOf[ExportedSymbol.Type],
      name  = "exportedType"
    ),
    new JsonSubTypes.Type(
      value = classOf[ExportedSymbol.Constructor],
      name  = "exportedConstructor"
    ),
    new JsonSubTypes.Type(
      value = classOf[ExportedSymbol.Method],
      name  = "exportedMethod"
    )
  )
)
sealed trait ExportedSymbol {
  def module: String

  def name: String

  def kind: Suggestion.Kind
}
object ExportedSymbol {

  /** Create [[ExportedSymbol]] from [[Suggestion]].
    *
    * @param suggestion the suggestion to convert
    * @return the corresponding [[ExportedSymbol]]
    */
  def fromSuggestion(suggestion: Suggestion): Option[ExportedSymbol] =
    suggestion match {
      case s: Suggestion.Module      => Some(Module(s.module))
      case s: Suggestion.Type        => Some(Type(s.module, s.name))
      case s: Suggestion.Constructor => Some(Constructor(s.module, s.name))
      case s: Suggestion.Method      => Some(Method(s.module, s.name))
      case _: Suggestion.Conversion  => None
      case _: Suggestion.Function    => None
      case _: Suggestion.Local       => None
    }

  /** Create an exported symbol of the suggestion module.
    *
    * @param suggestion the suggestion to convert
    * @return the corresponding [[ExportedSymbol.Module]]
    */
  def suggestionModule(suggestion: Suggestion): ExportedSymbol.Module =
    ExportedSymbol.Module(suggestion.module)

  /** The module symbol.
    *
    * @param module the module name
    */
  case class Module(module: String) extends ExportedSymbol {

    /** @inheritdoc */
    override def name: String =
      module

    /** @inheritdoc */
    override def kind: Suggestion.Kind =
      Suggestion.Kind.Module
  }

  /** The type symbol.
    *
    * @param module the module defining this atom
    * @param name the type name
    */
  case class Type(module: String, name: String) extends ExportedSymbol {

    /** @inheritdoc */
    override def kind: Suggestion.Kind =
      Suggestion.Kind.Type
  }

  /** The constructor symbol.
    *
    * @param module the module where this constructor is defined
    * @param name the constructor name
    */
  case class Constructor(module: String, name: String) extends ExportedSymbol {

    /** @inheritdoc */
    override def kind: Suggestion.Kind =
      Suggestion.Kind.Constructor
  }

  /** The method symbol.
    *
    * @param module the module defining this method
    * @param name the method name
    */
  case class Method(module: String, name: String) extends ExportedSymbol {

    /** @inheritdoc */
    override def kind: Suggestion.Kind =
      Suggestion.Kind.Method
  }
}
