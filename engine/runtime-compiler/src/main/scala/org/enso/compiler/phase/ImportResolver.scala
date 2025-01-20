package org.enso.compiler.phase

import org.enso.compiler.Compiler
import org.enso.compiler.context.CompilerContext.Module
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.MetadataStorage
import org.enso.compiler.core.ir.module.scope.{Export, Import}
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.common.CompilationStage

import scala.collection.mutable
import java.io.IOException
import java.util.logging.Level

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
          case ex: IOException =>
            context.logSerializationManager(
              Level.WARNING,
              "Deserialization of " + module.getName() + " failed",
              ex
            )
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

        val resolvedImports = importedModules.flatMap(_._2)

        val syntheticImports         = addSyntheticImports(current, resolvedImports)
        val resolvedSyntheticImports = syntheticImports.map(_._2)

        val newImportIRs =
          importedModules.map(_._1) ++ syntheticImports.map(_._1)

        currentLocal.resolvedImports =
          resolvedImports ++ resolvedSyntheticImports

        val newIr = ir.copy(imports = newImportIRs)
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
      currentLocal.resolvedImports.flatMap { resolvedImport =>
        val targetModules = resolvedImport.targets.map { target =>
          target.module.unsafeAsModule()
        }
        targetModules
      }.distinct
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
                val bm = current.getBindingsMap
                if (bm != null) {
                  val modulesFromResolvedImps = bm.resolvedImports.flatMap {
                    resolvedImp =>
                      resolvedImp.targets.map { target =>
                        target.module.unsafeAsModule()
                      }
                  }
                  (modulesFromResolvedImps.distinct, false)
                } else {
                  (Nil, false)
                }
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

  /** Traverses all the exports from the IR. Looks for exports that do not have associated imports.
    * Note that it is valid to export an entity via fully qualified name without importing it first
    * (at least in the current project).
    * @param module IR of the module.
    * @param resolvedImports List of all the resolved imports gathered so far from import IRs.
    * @return List of tuples - first is the synthetic import IR, second is its resolved import.
    */
  private def addSyntheticImports(
    module: Module,
    resolvedImports: List[BindingsMap.ResolvedImport]
  ): List[(Import, BindingsMap.ResolvedImport)] = {
    val resolvedImportNames = resolvedImports.map(_.importDef.name.name)
    val curModName          = module.getName.toString
    module.getIr.exports.flatMap {
      case Export.Module(
            expName,
            rename,
            onlyNames,
            _,
            isSynthetic,
            _
          ) if !isSynthetic =>
        val exportsItself = curModName.equals(expName.name)
        // Skip the exports that already have associated resolved import.
        if (!exportsItself && !resolvedImportNames.contains(expName.name)) {
          val syntheticImport = Import.Module(
            expName,
            rename,
            false,
            onlyNames,
            None,
            identifiedLocation = null,
            isSynthetic        = true,
            passData           = new MetadataStorage()
          )
          tryResolveImport(module.getIr, syntheticImport) match {
            case (_, Some(resolvedImp)) =>
              Some(
                (
                  syntheticImport,
                  resolvedImp
                )
              )
            case _ => None
          }
        } else {
          None
        }
      case _ => None
    }
  }

}
