package org.enso.compiler.phase

import org.enso.compiler.Compiler
import org.enso.compiler.context.CompilerContext.Module
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.module.scope.Import
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.polyglot.CompilationStage
import scala.collection.mutable
import java.io.IOException

/** Runs imports resolution. Starts from a given module and then recursively
  * collects all modules that are reachable from it.
  *
  * Each of the reachable modules will be parsed and will have imported modules
  * injected into its metadata. In effect, running this will bring every module
  * that could ever be necessary for the entry point compilation to at least
  * the [[CompilationStage.AFTER_IMPORT_RESOLUTION]] stage.
  *
  * @param compiler the compiler instance for the compiling context.
  */
final class ImportResolver(compiler: Compiler) extends ImportResolverForIR {

  /** Runs the import mapping logic.
    *
    * @param module the entry-point module.
    * @return a tuple containing a list of all modules that need to go through the full compilation pipeline and
    *          a list of all modules which have been inferred from bindings cache and could potentially be compiled lazily
    */
  def mapImports(
    module: Module,
    bindingsCachingEnabled: Boolean
  ): (List[Module], List[Module]) = {

    def analyzeModule(current: Module): List[Module] = {
      val context = compiler.context
      val (ir, currentLocal) =
        try {
          val ir = context.getIr(current)
          val currentLocal = ir.unsafeGetMetadata(
            BindingAnalysis,
            "Non-parsed module used in ImportResolver"
          )
          (ir, currentLocal)
        } catch {
          case _: IOException =>
            context.updateModule(
              current,
              u => {
                u.ir(null)
                u.compilationStage(CompilationStage.INITIAL)
                u.invalidateCache()
              }
            )
            compiler.ensureParsed(current, false)
            return analyzeModule(current)
        }
      // put the list of resolved imports in the module metadata
      if (
        context
          .getCompilationStage(current)
          .isBefore(
            CompilationStage.AFTER_IMPORT_RESOLUTION
          )
      ) {
        val importedModules: List[
          (Import, Option[BindingsMap.ResolvedImport])
        ] =
          ir.imports.map {
            case imp: Import.Module =>
              tryResolveImport(ir, imp)
            case other => (other, None)
          }
        currentLocal.resolvedImports = importedModules.flatMap(_._2)
        val newIr = ir.copy(imports = importedModules.map(_._1))
        context.updateModule(
          current,
          { u =>
            u.ir(newIr)
            if (!context.wasLoadedFromCache(current)) {
              u.compilationStage(
                CompilationStage.AFTER_IMPORT_RESOLUTION
              )
            }
          }
        )
      }
      currentLocal.resolvedImports
        .map(_.target.module.unsafeAsModule())
        .distinct
    }

    @scala.annotation.tailrec
    def go(
      stack: mutable.Stack[Module],
      seen: mutable.Set[Module],
      required: mutable.Set[Module]
    ): (List[Module], List[Module]) = {
      if (stack.isEmpty) {
        (required.toList, seen.toList diff required.toList)
      } else {
        val current = stack.pop()
        if (seen.contains(current)) {
          go(stack, seen, required)
        } else {
          val (next, isRequired) = if (bindingsCachingEnabled) {
            // Do we have bindings available for this modules' library?
            // - yes - extract the resolved imports but don't add them to the import/export resolution
            // - no - ensure they are parsed (load them from cache) and add them to the import/export resolution
            compiler.importExportBindings(current) match {
              case Some(bindings) =>
                compiler.context.updateModule(
                  current,
                  u => {
                    u.ir(bindings)
                    u.loadedFromCache(true)
                  }
                )
                val converted = Option(current.getBindingsMap())
                (
                  converted
                    .map(
                      _.resolvedImports
                        .map(_.target.module.unsafeAsModule())
                        .distinct
                    )
                    .getOrElse(Nil),
                  false
                )
              case None =>
                compiler.ensureParsed(current)
                (analyzeModule(current), true)
            }
          } else {
            compiler.ensureParsed(current)
            (analyzeModule(current), true)
          }
          // continue with updated stack
          go(
            stack.pushAll(next),
            seen += current,
            if (isRequired) { required += current }
            else required
          )
        }
      }
    }
    go(mutable.Stack(module), mutable.Set(), mutable.Set())
  }

  private[phase] def getCompiler(): Compiler = compiler

}
