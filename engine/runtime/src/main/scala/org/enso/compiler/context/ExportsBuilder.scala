package org.enso.compiler.context

import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{ExportedSymbol, ModuleExport}

final class ExportsBuilder {

  def build(moduleName: QualifiedName, ir: IR): Vector[ModuleExport] = {
    getBindings(ir).exportedSymbols.values.flatten
      .filter(_.module.getName != moduleName)
      .collect {
        case BindingsMap.ResolvedMethod(module, method) =>
          ExportedSymbol.ExportedMethod(module.getName.toString, method.name)
        case BindingsMap.ResolvedConstructor(module, cons) =>
          ExportedSymbol.ExportedAtom(module.getName.toString, cons.name)
        case BindingsMap.ResolvedModule(module) =>
          ExportedSymbol.ExportedModule(module.getName.toString)
      }
      .map(ModuleExport(moduleName.toString, _))
      .toVector
  }

  private def getBindings(ir: IR): BindingsMap =
    ir.unsafeGetMetadata(
      BindingAnalysis,
      "module without binding analysis in Exports Builder"
    )
}
