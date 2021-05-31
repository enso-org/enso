package org.enso.compiler.context

import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{ExportedSymbol, ModuleExport}

final class ExportsBuilder {

  def build(module: QualifiedName, ir: IR): Iterable[ModuleExport] = {
    getBindings(ir).exportedSymbols.values.flatten
      .flatMap {
        case BindingsMap.ResolvedMethod(module, method) =>
          Some(ExportedSymbol.ExportedMethod(module.getName, method.name))
        case BindingsMap.ResolvedConstructor(module, cons) =>
          Some(ExportedSymbol.ExportedAtom(module.getName, cons.name))
        case BindingsMap.ResolvedModule(module) =>
          Some(ExportedSymbol.ExportedModuleAtom(module.getName))
        case BindingsMap.ResolvedPolyglotSymbol(_, _) =>
          None
      }
      .map(ModuleExport(module, _))
  }

  def getBindings(ir: IR): BindingsMap =
    ir.unsafeGetMetadata(
      BindingAnalysis,
      "module without binding analysis in Exports Builder"
    )
}
