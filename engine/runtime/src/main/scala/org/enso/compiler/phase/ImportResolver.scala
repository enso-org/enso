package org.enso.compiler.phase

import org.enso.compiler.Compiler
import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.Module

import scala.collection.mutable

/** Runs imports resolution. Starts from a given module and then recursively
  * collects all modules that are reachable from it.
  *
  * Each of the reachable modules will be parsed and will have imported modules
  * injected into its metadata. In effect, running this will bring every module
  * that could ever be necessary for the entry point compilation to at least
  * the [[Module.CompilationStage.AFTER_IMPORT_RESOLUTION]] stage.
  *
  * @param compiler the compiler instance for the compiling context.
  */
class ImportResolver(compiler: Compiler) {

  /** Runs the import mapping logic.
    *
    * @param module the entry-point module.
    * @return a list of all modules that need to be compiled in order to run
    *         the program.
    */
  def mapImports(module: Module): List[Module] = {
    @scala.annotation.tailrec
    def go(
      stack: mutable.Stack[Module],
      seen: mutable.Set[Module]
    ): List[Module] = {
      if (stack.isEmpty) {
        seen.toList
      } else {
        val current = stack.pop()
        if (seen.contains(current)) {
          go(stack, seen)
        } else {
          // get module metadata
          compiler.ensureParsed(current)
          val ir = current.getIr
          val currentLocal = ir.unsafeGetMetadata(
            BindingAnalysis,
            "Non-parsed module used in ImportResolver"
          )
          // put the list of resolved imports in the module metadata
          if (
            current.getCompilationStage
              .isBefore(Module.CompilationStage.AFTER_IMPORT_RESOLUTION)
          ) {
            val importedModules = ir.imports.flatMap {
              case imp: IR.Module.Scope.Import.Module =>
                val impName = imp.name.name
                val exp = ir.exports
                  .collect { case ex: IR.Module.Scope.Export.Module => ex }
                  .find(_.name.name == impName)
                compiler
                  .getModule(impName)
                  .map(BindingsMap.ResolvedImport(imp, exp, _))
              case _ => None
            }
            currentLocal.resolvedImports = importedModules
            current.unsafeSetCompilationStage(
              Module.CompilationStage.AFTER_IMPORT_RESOLUTION
            )
          }
          // continue with updated stack
          go(
            stack.pushAll(currentLocal.resolvedImports.map(_.module)),
            seen += current
          )
        }
      }
    }

    go(mutable.Stack(module), mutable.Set())
  }

}
