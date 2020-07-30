package org.enso.compiler.phase

import org.enso.compiler.Compiler
import org.enso.compiler.core.IR
import org.enso.compiler.pass.analyse.BindingResolution
import org.enso.interpreter.runtime.Module
import util.control.Breaks._

import scala.collection.mutable

class ImportResolver(compiler: Compiler) {
  def mapImports(module: Module): List[Module] = {
    val seen: mutable.Set[Module] = mutable.Set()
    var stack: List[Module]       = List(module)
    while (stack.nonEmpty) {
      val current = stack.head
      stack = stack.tail
      breakable {
        if (
          seen.contains(current) || current.getCompilationStage.isAtLeast(
            Module.CompilationStage.IMPORTS_RESOLVED
          )
        ) {
          break()
        }
        compiler.ensureParsed(current)
        val ir = current.getIr
        val currentLocal = ir.unsafeGetMetadata(
          BindingResolution,
          "Non-parsed module used in ImportResolver"
        )
        val importedModuleNames = ir.imports.collect {
          case imp: IR.Module.Scope.Import.Module => imp.name
        }
        // TODO[MK] COMPLAIN ABOUT MISSING
        val importedModules = importedModuleNames.flatMap(compiler.getModule)
        // TODO[MK] Remove when No Implicit Prelude
        currentLocal.resolvedImports = compiler.context.getTopScope.getBuiltins.getModule :: importedModules
        current.setCompilationStage(Module.CompilationStage.IMPORTS_RESOLVED)
        seen += current
        stack = importedModules ++ stack
      }
    }
    seen.toList
  }
}
