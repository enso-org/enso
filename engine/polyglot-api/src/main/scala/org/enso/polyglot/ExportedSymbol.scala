package org.enso.polyglot

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(
      value = classOf[ExportedSymbol.ExportedModule],
      name  = "exportedModule"
    ),
    new JsonSubTypes.Type(
      value = classOf[ExportedSymbol.ExportedAtom],
      name  = "exportedAtom"
    ),
    new JsonSubTypes.Type(
      value = classOf[ExportedSymbol.ExportedMethod],
      name  = "exportedMethod"
    )
  )
)
sealed trait ExportedSymbol {
  def module: String
}
object ExportedSymbol {

  case class ExportedModule(module: String) extends ExportedSymbol

  case class ExportedAtom(module: String, atom: String) extends ExportedSymbol

  case class ExportedMethod(module: String, method: String)
      extends ExportedSymbol
}
