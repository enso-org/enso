package org.enso.compiler.context

import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{ExportedSymbol, ModuleExports}

final class ExportsBuilder {

  def build(moduleName: QualifiedName, ir: IR): ModuleExports = {
    val symbols = getBindings(ir).exportedSymbols.values.flatten
      .filter(_.module.getName != moduleName)
      .collect {
        case BindingsMap.ResolvedMethod(module, method) =>
          ExportedSymbol.Method(module.getName.toString, method.name)
        case BindingsMap.ResolvedConstructor(module, cons) =>
          ExportedSymbol.Atom(module.getName.toString, cons.name)
        case BindingsMap.ResolvedModule(module) =>
          ExportedSymbol.Module(module.getName.toString)
      }
      .toSet[ExportedSymbol]
    ModuleExports(moduleName.toString, symbols)
  }

  private def getBindings(ir: IR): BindingsMap =
    ir.unsafeGetMetadata(
      BindingAnalysis,
      "module without binding analysis in Exports Builder"
    )
}
