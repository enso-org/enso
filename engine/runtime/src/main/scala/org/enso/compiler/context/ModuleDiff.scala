package org.enso.compiler.context

import org.enso.compiler.Compiler
import org.enso.compiler.core.IR
import org.enso.compiler.phase.{ExportsResolution, ImportResolver}
import org.enso.interpreter.runtime.Module
import org.enso.pkg.QualifiedName
import org.enso.polyglot.Suggestion
import org.enso.polyglot.runtime.Runtime.Api

class ModuleDiff(compiler: Compiler) {

  private val importResolver = new ImportResolver(compiler)

  def reexports(module: Module): Seq[QualifiedName] = {
    val imported = importResolver.mapImports(module)
    ModuleDiff.reexports(imported)(module)
  }

  def exportedBy(suggestion: Suggestion): QualifiedName = ???

  def diffExports(a: Module, b: Module): Seq[Api.SuggestionUpdate] = ???
}

object ModuleDiff {

  def defines(module: Module): Seq[QualifiedName] =
    module.getIr.bindings.flatMap {
      case IR.Module.Scope.Definition.Atom(name, _, _, _, _) =>
        Some(module.getName.createChild(name.name))

      case IR.Module.Scope.Definition.Method
            .Explicit(IR.Name.MethodReference(_, name, _, _, _), _, _, _, _) =>
        Some(module.getName.createChild(name.name))

      case _ =>
        None
    }

  def reexports(imported: List[Module])(module: Module): Seq[QualifiedName] = {
    val modules = new ExportsResolution().run(imported)
    val exported = module.getIr.exports.map { export =>
      val parts = export.name.parts.map(_.name)
      export.rename
        .map(n => QualifiedName(parts.init, n.name))
        .getOrElse(QualifiedName(parts.init, parts.last))
    }
    val exp1 = module.getIr.exports.map { export =>
      s"[${module.getName}] from ${export.name.name} as ${export.rename.map(
        _.name
      )} export (all=${export.isAll}) ${export.onlyNames.map(_.map(_.name))}"
    }
    println(exp1.mkString("\n"))
    val exportedModules = modules.filter { module =>
      exported.contains(module.getName)
    }
    exportedModules.map(_.getName)
  }

}
