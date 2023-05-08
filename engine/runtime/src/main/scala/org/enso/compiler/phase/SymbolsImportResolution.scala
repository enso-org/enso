package org.enso.compiler.phase

import com.oracle.truffle.api.TruffleLogger
import org.enso.compiler.Compiler
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Import
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{ImportTarget, ModuleReference}
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.Module

import java.util.logging.Level

class SymbolsImportResolution(compiler: Compiler) {
  private val logger: TruffleLogger = compiler.context.getLogger(getClass)

  def resolveImportSymbols(
    module: Module
  ): Unit = {
    val bindingMap = module.getIr.unsafeGetMetadata(
      BindingAnalysis,
      "Should be analyzed before resolving import symbols"
    )
    logger.log(
      Level.FINE,
      "Resolving import symbols for module {0}",
      Array[Object](module.getName.toString)
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
            importedName,
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
        logger.log(
          Level.FINER,
          s"Resolving symbols [{0}] from import `{1}`",
          Array[Object](onlyNames.map(_.name), imp.showCode())
        )

        // Check if the import is from a type rather than from a module, i.e.,
        // `from Module.Type import Constructor` versus `from Module import Some_Type`.
        val shouldImportFromType = compiler.getModule(importedName.name).isEmpty

        val moduleName = if (shouldImportFromType) {
          importedName.parts.dropRight(1).map(_.name).mkString(".")
        } else {
          importedName.name
        }
        val typeName: Option[String] = if (shouldImportFromType) {
          Some(importedName.parts.last.name)
        } else {
          None
        }

        logger.log(
          Level.FINER,
          "moduleName = {0}, typeName = {1}",
          Array[Object](moduleName, typeName)
        )

        val importedModule = compiler.getModule(moduleName) match {
          case Some(module) => module
          case None =>
            throw new CompilerError(
              s"Module ${moduleName} should already be imported"
            )
        }

        val importedModuleBindingMap =
          importedModule.getIr.unsafeGetMetadata(
            BindingAnalysis,
            "Should be analyzed before resolving import symbols"
          )
        val symbolsResolution =
          tryResolveSymbols(
            importedModule,
            importedModuleBindingMap,
            onlyNames,
            typeName
          )

        logger.log(
          Level.FINER,
          "Got result from symbolsResolution: Resolved = {0}, Unresolved = {1}",
          Array[Object](
            symbolsResolution.resolved.map(_.qualifiedName.toString),
            symbolsResolution.unresolved.map(_.showCode())
          )
        )

        if (symbolsResolution.unresolved.nonEmpty) {
          // Replace the IR with IR.Error
          IR.Error.ImportExport(
            ir = imp,
            reason = IR.Error.ImportExport.SymbolsDoNotExist(
              symbolNames = symbolsResolution.unresolved.map(_.name),
              moduleName  = importedName.toString
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
    symbolsToImport: List[IR.Name.Literal],
    typeName: Option[String]
  ): SymbolsResolution = {
    val analysedSymbols: List[Either[ImportTarget, IR.Name.Literal]] =
      symbolsToImport
        .map(symbolToImport => {
          tryResolveFromDefinedEntities(
            module,
            bindingsMap,
            symbolToImport,
            typeName
          ) match {
            case Some(importTarget) => Left(importTarget)
            case None =>
              tryResolveFromExportedSymbols(bindingsMap, symbolToImport, typeName) match {
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

  /**
   * Tries to resolve a symbol (of a type or a module) from defined entities within
   * the given `bindingMap`.
   * @param symbolToImport Symbol to import from a type or from a module
   * @param typeName If defined, a member from a type is imported, rather than a member of a module.
   * @return
   */
  private def tryResolveFromDefinedEntities(
    module: Module,
    bindingMap: BindingsMap,
    symbolToImport: IR.Name.Literal,
    typeName: Option[String]
  ): Option[ImportTarget] = {
    if (typeName.isDefined) {
      // A member from a type is imported
      val foundType: Option[BindingsMap.Type] =
        bindingMap.definedEntities.find(_.name == typeName.get) match {
          case Some(definedEntity) =>
            definedEntity match {
              case tp: BindingsMap.Type => Some(tp)
              case _ => throw new CompilerError(s"Expected BindingsMap.Type, got $definedEntity")
            }
          case None => None
      }
      // TODO: Resolve member
      foundType match {
        case Some(tp) =>
          logger.log(
            Level.FINER,
            "Resolved type {0} from definedEntities in module {1}",
            Array[Object](tp, module)
          )
          Some(
            BindingsMap.ResolvedType(ModuleReference.Concrete(module), tp)
          )
        case None => None
      }
    } else {
      bindingMap.definedEntities.find(_.name == symbolToImport.name) match {
        case Some(definedEntity) =>
          definedEntity match {
            case tp: BindingsMap.Type =>
              logger.log(
                Level.FINER,
                "Resolved type {0} from definedEntities in module {1}",
                Array[Object](tp, module)
              )
              Some(
                BindingsMap.ResolvedType(ModuleReference.Concrete(module), tp)
              )
            case _: BindingsMap.ModuleMethod =>
              // TODO: Resolve to a method, not to a module
              Some(
                BindingsMap.ResolvedModule(
                  ModuleReference.Concrete(module)
                )
              )
            case BindingsMap.PolyglotSymbol(_) =>
              // TODO: Resolve to a polyglot symbol, not to a module
              Some(
                BindingsMap.ResolvedModule(
                  ModuleReference.Concrete(module)
                )
              )
          }
        case None => None
      }
    }
  }

  /**
   * Tries to resolve a symbol (of a type or a module) from exported symbols within
   * the given `bindingMap`.
   * @param bindingMap
   * @param symbolToImport Symbol to import from a type or from a module
   * @param typeName If defined, a member from a type is imported, rather than a member of a module.
   * @return
   */
  private def tryResolveFromExportedSymbols(
    bindingMap: BindingsMap,
    symbolToImport: IR.Name.Literal,
    typeName: Option[String]
  ): Option[ImportTarget] = {
    val exportedNamesToSearch: Option[List[BindingsMap.ResolvedName]] =
      if (typeName.isDefined) {
        bindingMap.exportedSymbols.get(typeName.get)
      } else {
        bindingMap.exportedSymbols.get(symbolToImport.name)
      }
    exportedNamesToSearch match {
      case Some(resolvedNames) =>
        logger.log(
          Level.FINER,
          "Found symbols [{0}] from exportedSymbols in bindings map from module {1}",
          Array[Object](resolvedNames, bindingMap.currentModule.getName.toString)
        )
        val resolvedImportTargets: List[ImportTarget] =
          resolvedNames.collect {
            case resolvedModule: BindingsMap.ResolvedModule => resolvedModule
            case resolvedType: BindingsMap.ResolvedType => resolvedType
            case BindingsMap.ResolvedConstructor(resolvedType, _) => resolvedType
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
