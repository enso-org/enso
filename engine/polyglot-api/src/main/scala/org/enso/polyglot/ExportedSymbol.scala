package org.enso.polyglot

import org.enso.pkg.QualifiedName

sealed trait ExportedSymbol {
  def module: QualifiedName
}
object ExportedSymbol {

  case class ExportedMethod(module: QualifiedName, method: String)
      extends ExportedSymbol

  case class ExportedAtom(module: QualifiedName, atom: String)
      extends ExportedSymbol

  case class ExportedModuleAtom(module: QualifiedName) extends ExportedSymbol
}
