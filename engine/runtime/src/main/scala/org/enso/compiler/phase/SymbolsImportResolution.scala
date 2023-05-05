package org.enso.compiler.phase

import org.enso.compiler.Compiler
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Import
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{ImportTarget, ModuleReference}
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.Module

class SymbolsImportResolution(compiler: Compiler) {
  def resolveImportSymbols(
    module: Module
  ): Unit = {
    val bindingMap = module.getIr.unsafeGetMetadata(
      BindingAnalysis,
      "Should be analyzed before resolving import symbols"
    )
    module.unsafeSetIr(
      module.getIr.copy(
        imports = module.getIr.imports.map(analyseImport(bindingMap, _))
      )
    )
  }

  private def analyseImport(
    bindingsMap: BindingsMap,
    imp: IR.Module.Scope.Import
  ): IR.Module.Scope.Import = {
    imp match {
      case imp @ Import.Module(
            importedModuleName,
            _,
            _,
            Some(onlyNames),
            _,
            _,
            _,
            _,
            _
          ) =>
        assert(
          !bindingsMap.resolvedImports.contains(imp),
          "From imports should not be resolved yet"
        )
        // Get resolved module from bindings map
        val importedModule = compiler
          .getModule(importedModuleName.name)
          .getOrElse(
            throw new CompilerError(
              s"Module ${importedModuleName.name} should already be imported"
            )
          )
        val importedModuleBindingMap =
          importedModule.getIr.unsafeGetMetadata(
            BindingAnalysis,
            "Should be analyzed before resolving import symbols"
          )
        val symbolsResolution =
          tryResolveSymbols(importedModule, importedModuleBindingMap, onlyNames)
        if (symbolsResolution.unresolved.nonEmpty) {
          // Replace the IR with IR.Error
          IR.Error.ImportExport(
            ir = imp,
            reason = IR.Error.ImportExport.SymbolsDoNotExist(
              symbolNames = symbolsResolution.unresolved.map(_.name),
              moduleName  = importedModuleName.toString
            )
          )
        } else {

          val resolvedImports =
            symbolsResolution.resolved.map(importTarget => {
              BindingsMap.ResolvedImport(
                importDef = imp,
                exports   = List.empty,
                target    = importTarget
              )
            })

          bindingsMap.resolvedImports =
            bindingsMap.resolvedImports.appendedAll(resolvedImports)
          imp
        }
      case _ => imp
    }
  }

  private def tryResolveSymbols(
    module: Module,
    bindingsMap: BindingsMap,
    symbolsToImport: List[IR.Name.Literal]
  ): SymbolsResolution = {
    val analysedSymbols: List[Either[ImportTarget, IR.Name.Literal]] =
      symbolsToImport
        .map(symbolToImport => {
          tryResolveFromDefinedEntities(
            module,
            bindingsMap,
            symbolToImport
          ) match {
            case Some(importTarget) => Left(importTarget)
            case None =>
              tryResolveFromExportedSymbols(bindingsMap, symbolToImport) match {
                case Some(importTarget) => Left(importTarget)
                case None               => Right(symbolToImport)
              }
          }
        })

    val unresolvedSymbols: List[IR.Name.Literal] =
      analysedSymbols.collect { case Right(name) => name }
    val resolvedSymbols: List[ImportTarget] =
      analysedSymbols.collect { case Left(importTarget) => importTarget }

    SymbolsResolution(resolvedSymbols, unresolvedSymbols)
  }

  private def tryResolveFromDefinedEntities(
    module: Module,
    bindingMap: BindingsMap,
    symbolToImport: IR.Name.Literal
  ): Option[ImportTarget] = {
    bindingMap.definedEntities.find(_.name == symbolToImport.name) match {
      case Some(definedEntity) =>
        definedEntity match {
          case tp: BindingsMap.Type =>
            Some(
              BindingsMap.ResolvedType(ModuleReference.Concrete(module), tp)
            )
          case moduleMethod: BindingsMap.ModuleMethod =>
            throw new CompilerError(
              s"Resolving method ${moduleMethod} is not yet supported"
            )
          case BindingsMap.PolyglotSymbol(name) =>
            throw new CompilerError(
              s"Resolving polyglot symbol '$name' is not yet supported'"
            )
        }
      case None => None
    }
  }

  private def tryResolveFromExportedSymbols(
    bindingMap: BindingsMap,
    symbolToImport: IR.Name.Literal
  ): Option[ImportTarget] = {
    bindingMap.exportedSymbols.get(symbolToImport.name) match {
      case Some(resolvedNames) =>
        val resolvedImportTargets: List[ImportTarget] =
          resolvedNames.collect {
            case resolvedModule: BindingsMap.ResolvedModule => resolvedModule
            case resolvedType: BindingsMap.ResolvedType     => resolvedType
          }
        // Take the first resolvedName that is also ImportTarget
        resolvedImportTargets.headOption
      case None => None
    }
  }

  private case class SymbolsResolution(
    resolved: List[ImportTarget],
    unresolved: List[IR.Name.Literal]
  )
}
