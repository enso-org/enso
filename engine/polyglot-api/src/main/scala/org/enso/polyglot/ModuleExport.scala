package org.enso.polyglot

import org.enso.pkg.QualifiedName

/** The `module` that exports `symbol`. */
case class ModuleExport(module: QualifiedName, symbol: ExportedSymbol)
