package org.enso.compiler.phase

import org.enso.compiler.Compiler
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.{Export, Import}
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{
  ModuleReference,
  ResolvedModule,
  ResolvedType,
  Type
}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.editions.LibraryName
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
  import ImportResolver._

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
      val ir = current.getIr
      val currentLocal = ir.unsafeGetMetadata(
        BindingAnalysis,
        "Non-parsed module used in ImportResolver"
      )
      // put the list of resolved imports in the module metadata
      if (
        current.getCompilationStage
          .isBefore(
            Module.CompilationStage.AFTER_IMPORT_RESOLUTION
          ) || !current.hasCrossModuleLinks
      ) {
        val importedModules: List[
          (IR.Module.Scope.Import, Option[BindingsMap.ResolvedImport])
        ] =
          ir.imports.map {
            case imp: IR.Module.Scope.Import.Module =>
              tryResolveImport(ir, imp)
            case other => (other, None)
          }
        currentLocal.resolvedImports = importedModules.flatMap(_._2)
        val newIr = ir.copy(imports = importedModules.map(_._1))
        current.unsafeSetIr(newIr)
        if (!current.wasLoadedFromCache()) {
          current.unsafeSetCompilationStage(
            Module.CompilationStage.AFTER_IMPORT_RESOLUTION
          )
        }
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
                val converted = bindings
                  .toConcrete(compiler.packageRepository.getModuleMap)
                  .map { concreteBindings =>
                    concreteBindings
                  }
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

  private def tryResolveAsType(
    name: IR.Name.Qualified
  ): Option[ResolvedType] = {
    val tp  = name.parts.last.name
    val mod = name.parts.dropRight(1).map(_.name).mkString(".")
    compiler.getModule(mod).flatMap { mod =>
      compiler.ensureParsed(mod)
      mod.getIr
        .unsafeGetMetadata(
          BindingAnalysis,
          "impossible: just ensured it's parsed"
        )
        .definedEntities
        .find(_.name == tp)
        .collect { case t: Type =>
          ResolvedType(ModuleReference.Concrete(mod), t)
        }
    }
  }

  private def tryResolveImport(
    module: IR.Module,
    imp: Import.Module
  ): (IR.Module.Scope.Import, Option[BindingsMap.ResolvedImport]) = {
    val impName = imp.name.name
    val exp = module.exports
      .collect { case ex: Export.Module if ex.name.name == impName => ex }
    val fromAllExports = exp.filter(_.isAll)
    fromAllExports match {
      case _ :: _ :: _ =>
        // Detect potential conflicts when importing all and hiding names for the exports of the same module
        val unqualifiedImports = fromAllExports.collect {
          case e if e.onlyNames.isEmpty => e
        }
        val qualifiedImports = fromAllExports.collect {
          case IR.Module.Scope.Export.Module(
                _,
                _,
                _,
                Some(onlyNames),
                _,
                _,
                _,
                _,
                _
              ) =>
            onlyNames.map(_.name)
        }
        val importsWithHiddenNames = fromAllExports.collect {
          case e @ IR.Module.Scope.Export.Module(
                _,
                _,
                _,
                _,
                Some(hiddenNames),
                _,
                _,
                _,
                _
              ) =>
            (e, hiddenNames)
        }
        importsWithHiddenNames.foreach { case (e, hidden) =>
          val unqualifiedConflicts = unqualifiedImports.filter(_ != e)
          if (unqualifiedConflicts.nonEmpty) {
            throw HiddenNamesShadowUnqualifiedExport(
              e.name.name,
              hidden.map(_.name)
            )
          }

          val qualifiedConflicts =
            qualifiedImports
              .filter(_ != e)
              .flatten
              .intersect(hidden.map(_.name))
          if (qualifiedConflicts.nonEmpty) {
            throw HiddenNamesShadowQualifiedExport(
              e.name.name,
              qualifiedConflicts
            )
          }
        }
      case _ =>
    }
    val libraryName = imp.name.parts match {
      case namespace :: name :: _ =>
        LibraryName(namespace.name, name.name)
      case _ =>
        throw new CompilerError(
          "Imports should contain at least two segments after " +
          "desugaring."
        )
    }
    compiler.packageRepository
      .ensurePackageIsLoaded(libraryName) match {
      case Right(()) =>
        compiler.getModule(impName) match {
          case Some(module) =>
            (
              imp,
              Some(
                BindingsMap.ResolvedImport(
                  imp,
                  exp,
                  ResolvedModule(ModuleReference.Concrete(module))
                )
              )
            )
          case None =>
            tryResolveAsType(imp.name) match {
              case Some(tp) =>
                (imp, Some(BindingsMap.ResolvedImport(imp, exp, tp)))
              case None =>
                (
                  IR.Error.ImportExport(
                    imp,
                    IR.Error.ImportExport.ModuleDoesNotExist(impName)
                  ),
                  None
                )
            }
        }
      case Left(loadingError) =>
        (
          IR.Error.ImportExport(
            imp,
            IR.Error.ImportExport.PackageCouldNotBeLoaded(
              impName,
              loadingError.toString
            )
          ),
          None
        )
    }
  }
}

object ImportResolver {
  trait HiddenNamesConflict {
    def getMessage(): String
  }

  private case class HiddenNamesShadowUnqualifiedExport(
    name: String,
    hiddenNames: List[String]
  ) extends Exception(
        s"""Hidden '${hiddenNames.mkString(",")}' name${if (
          hiddenNames.size == 1
        ) ""
        else
          "s"} of the export module ${name} conflict${if (hiddenNames.size == 1)
          "s"
        else
          ""} with the unqualified export"""
      )
      with HiddenNamesConflict

  private case class HiddenNamesShadowQualifiedExport(
    name: String,
    conflict: List[String]
  ) extends Exception(
        s"""Hidden '${conflict.mkString(",")}' name${if (conflict.size == 1) ""
        else
          "s"} of the exported module ${name} conflict${if (conflict.size == 1)
          "s"
        else
          ""} with the qualified export"""
      )
      with HiddenNamesConflict
}
