package org.enso.compiler.phase

import com.oracle.truffle.api.TruffleLogger
import org.enso.compiler.Compiler
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.{Definition, Import}
import org.enso.compiler.core.IR.Name
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{ImportTarget, ModuleReference}
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.Module

import java.util.logging.Level
import scala.collection.mutable.ListBuffer

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
          "From import statements should not be resolved yet"
        )
        logger.log(
          Level.FINER,
          s"Resolving symbols [{0}] from import `{1}`",
          Array[Object](onlyNames.map(_.name), imp.showCode())
        )
        tryResolveSymbolsAndUpdateBindingMap(
          imp,
          bindingsMap,
          importedName,
          Some(onlyNames)
        )

      case imp@Import.Module(
        importedName,
        _,
        isAll,
        None,
        _,
        _,
        _,
        _,
        _
        ) if isAll =>
        logger.log(
          Level.FINER,
          "Resolving all symbols from import `{0}`",
          Array[Object](imp.showCode())
        )
        tryResolveSymbolsAndUpdateBindingMap(
          imp,
          bindingsMap,
          importedName,
          None
        )
      case _ => imp
    }
  }

  /**
   * Tries to resolve symbols from the [[module]], potentially from the [[typeName]] if set.
   * @param module Module from which symbols are imported.
   * @param bindingsMap BindingsMap associated with the [[module]].
   * @param symbolsToImport All the symbols to be imported
   * @param typeName The name of the type from which the symbols are imported. If None,
   *                 the symbols are imported from the module.
   */
  private def tryResolveSymbols(
    module: Module,
    bindingsMap: BindingsMap,
    symbolsToImport: List[IR.Name.Literal],
    typeName: Option[String]
  ): SymbolsResolution = {
    val resolvedSymbols: ListBuffer[ImportTarget] = ListBuffer.empty
    for (symbolToImport <- symbolsToImport) {
      tryResolveFromDefinedEntities(
        module,
        bindingsMap,
        symbolToImport,
        typeName
      ) match {
        case Left(importTarget) => resolvedSymbols += importTarget
        case Right(_) =>
          tryResolveFromExportedSymbols(module, symbolToImport, typeName) match {
            case Left(importTarget) => resolvedSymbols += importTarget
            case Right(errorReason) =>
              // Report failure
              return SymbolsResolution(
                resolvedSymbols.toList,
                Some(errorReason)
              )
          }
      }
    }
    SymbolsResolution(
      resolvedSymbols.toList,
      None
    )
  }

  /**
   * From the given module bindings, finds all the static module method and type definitions.
   */
  private def findAllStaticMethodsAndTypesDefinedInModule(
    moduleBindings: List[IR.Module.Scope.Definition]
  ): List[IR.Name.Literal] = {
    logger.log(
      Level.FINER,
      "Finding all static methods and types in module"
    )
    val symbolsToImport: ListBuffer[IR.Name.Literal] = ListBuffer.empty
    moduleBindings.foreach {
      case Definition.Method.Explicit(methodRef, _, _, _, _)
        if methodRef.methodName.isInstanceOf[Name.Literal] =>
        symbolsToImport += methodRef.methodName.asInstanceOf[Name.Literal]
      case Definition.Method.Binding(methodRef, _, _, _, _, _)
        if methodRef.methodName.isInstanceOf[Name.Literal] =>
        symbolsToImport += methodRef.methodName.asInstanceOf[Name.Literal]
      case Definition.Type(name, _, _, _, _, _)
        if name.isInstanceOf[Name.Literal] =>
        symbolsToImport += name.asInstanceOf[Name.Literal]
      case _ => ()
    }
    symbolsToImport.toList
  }

  private def findAllExportedSymbolsFromModule(
    importedModuleBindingMap: BindingsMap
  ): List[BindingsMap.ImportTarget] = {
    val exportedSymbols: ListBuffer[BindingsMap.ImportTarget] = ListBuffer.empty
    importedModuleBindingMap.exportedSymbols.foreach {
      case (_, resolvedNames) =>
        resolvedNames.foreach {
          case resolvedType: BindingsMap.ResolvedType => exportedSymbols += resolvedType
          case resolvedModule: BindingsMap.ResolvedModule => exportedSymbols += resolvedModule
          case resolvedMethod: BindingsMap.ResolvedMethod => exportedSymbols += resolvedMethod
          case resolvedCtor: BindingsMap.ResolvedConstructor => exportedSymbols += resolvedCtor
          case _ => ()
        }
    }
    exportedSymbols.toList
  }

  /**
   * Finds all teh constructors from a type. Returns empty list if there is no such type,
   * or if the type does not have any constructors.
   * @param moduleBindings
   * @param typeName
   * @return
   */
  private def findAllConstructorsFromType(
    moduleBindings: List[IR.Module.Scope.Definition],
    typeName: String
  ): List[IR.Name.Literal] = {
    logger.log(
      Level.FINEST,
      "Finding all constructors from type `{0}`",
      Array[Object](typeName)
    )
    val ctors: ListBuffer[IR.Name.Literal] = ListBuffer.empty
    moduleBindings.foreach {
      case Definition.Type(name, _, members, _, _, _) if name.name == typeName =>
        members.foreach(_.name match {
            case ctorName: IR.Name.Literal => ctors += ctorName
            case _                         => ()
          }
        )
      case _ => ()
    }
    // ctors buffer might be empty, in case the type is not in the module.
    // That is fine, the error will be reported later.
    ctors.toList
  }

  /**
   * TODO: Reword
   * Tries to resolve all the symbols given in [[symbolsToImport]] from the module
   * `importedModule`, potentially from the type given in `typeName` and updates
   * the imported module's BindingMap accordingly.
   *
   * In case of a failed resolution, an import error reason IR is returned and no
   * metadata is updated. When the resolution is successful, the given binding map
   * is updated and the [[importStatement]] is returned.
   *
   *
   * @param importStatement The import statement that is being analyzed
   * @param bindingMap The binding map of the current module from which we are analyzing
   *                   the import. This binding map may be updated as a result of this
   *                   operation.
   * @param symbolsToImport The symbols that are being imported from the module or from the type.
   *                        If None, all the symbols should be imported.
   * @return Either the import statement given in `importStatement` in case that all the
   *         symbols were successfuly resolved, or an import error reason IR when some
   *         symbols failed to resolve.
   */
  private def tryResolveSymbolsAndUpdateBindingMap(
    importStatement: IR.Module.Scope.Import.Module,
    bindingMap: BindingsMap,
    importedName: IR.Name.Qualified,
    symbolsToImport: Option[List[IR.Name.Literal]],
  ): IR.Module.Scope.Import = {
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
      Level.FINEST,
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

    val importedBindingMap =
      importedModule.getIr.unsafeGetMetadata(BindingAnalysis, "BindingMap should be present")

    val symbolsToImportUnwrapped: List[IR.Name.Literal] = {
      symbolsToImport match {
        case Some(symbols) => symbols
        case None =>
          typeName match {
            case Some(typeNameUnwrapped) =>
              findAllConstructorsFromType(
                importedModule.getIr.bindings,
                typeNameUnwrapped
              )
            case None    =>
              // TODO: Find also all exported symbols from importedModule
              findAllStaticMethodsAndTypesDefinedInModule(
                importedModule.getIr.bindings
              )
          }
      }
    }

    logger.log(
      Level.FINER,
      "Trying to resolve symbols {0} from type {1} from module {2}",
      Array[Object](symbolsToImportUnwrapped, typeName, importedModule.getName)
    )

    val symbolsResolution = {
      // Trying to import any symbols from a non-existing type is an error
      if (typeName.isDefined && !typeExists(typeName.get, importedBindingMap)) {
        SymbolsResolution(
          resolved = List.empty,
          errorReason = Some(
            IR.Error.ImportExport.TypeDoesNotExist(
              typeName.get,
              moduleName
            )
          )
        )
      } else {
        tryResolveSymbols(
          importedModule,
          importedBindingMap,
          symbolsToImportUnwrapped,
          typeName
        )
      }
    }

    logger.log(
      Level.FINER,
      "Got result from symbolsResolution: Resolved = {0}, Error = {1}",
      Array[Object](
        symbolsResolution.resolved.map(_.qualifiedName.toString),
        symbolsResolution.errorReason
      )
    )

    if (symbolsResolution.errorReason.isDefined) {
      // Replace the IR with IR.Error and don't update the binding map
      IR.Error.ImportExport(
        ir = importStatement,
        reason = symbolsResolution.errorReason.get
      )
    } else {
      val importingAllSymbols = symbolsToImport.isEmpty

      val allResolvedSymbols: List[ImportTarget] =
        if (importingAllSymbols) {
          // Append all exported symbols from the module to the resolved imports.
          // These are transitively imported symbols
          val additionalExportedSymbols = findAllExportedSymbolsFromModule(importedBindingMap)

          // Filter away all the symbols that are already resolved.
          val filteredAdditionalExportedSymbols: List[ImportTarget] =
            additionalExportedSymbols.filterNot(
              additionalExportedSymbol =>
                symbolsResolution.resolved.exists(_.qualifiedName == additionalExportedSymbol.qualifiedName)
            )

          logger.log(
            Level.FINEST,
            "Transitive imports that will be added to the module (exported symbols from imported module): {0}",
            Array[Object](filteredAdditionalExportedSymbols)
          )

          symbolsResolution.resolved ++ filteredAdditionalExportedSymbols
        } else {
          symbolsResolution.resolved
        }

      val resolvedImports =
        allResolvedSymbols.map(importTarget => {
          BindingsMap.ResolvedImport(
            importDef = importStatement,
            exports = List.empty,
            target = importTarget
          )
        })

      bindingMap.resolvedImports =
        bindingMap.resolvedImports.appendedAll(resolvedImports)
      importStatement
    }
  }

  /**
   * Tries to resolve a symbol (of a type or a module) from defined entities within
   * the given `bindingMap`.
   * @param symbolToImport Symbol to import from a type or from a module
   * @param typeName If defined, a member from a type is imported, rather than a member of a module.
   * @return ImportTarget in case of successful resolution or an error reason
   */
  private def tryResolveFromDefinedEntities(
    module: Module,
    bindingMap: BindingsMap,
    symbolToImport: IR.Name.Literal,
    typeName: Option[String]
  ): Either[ImportTarget, IR.Error.ImportExport.Reason] = {
    logger.log(
      Level.FINEST,
      "Trying to resolve symbol {0} from type {1} from module {2} defined entities",
      Array[Object](symbolToImport, typeName, module.getName)
    )
    if (typeName.isDefined) {
      // A member `symbolToImport` from a type `typeName` is imported from `module`.
      val foundType: BindingsMap.Type =
        bindingMap.definedEntities.find(_.name == typeName.get) match {
          case Some(definedEntity) =>
            definedEntity match {
              case tp: BindingsMap.Type => tp
              case _ => throw new CompilerError(s"Expected BindingsMap.Type, got $definedEntity")
            }
          case None =>
            return Right(
              IR.Error.ImportExport.TypeDoesNotExist(typeName.get, module.getName.toString)
            )
        }
      // Resolve member of a type
      foundType.members.find(_.name == symbolToImport.name) match {
        case Some(constructor) =>
          logger.log(
            Level.FINER,
            "Resolved constructor {0} from type {1} from definedEntities in module {2}",
            Array[Object](constructor, foundType, module)
          )
          Left(
            BindingsMap.ResolvedConstructor(
              BindingsMap.ResolvedType(ModuleReference.Concrete(module), foundType),
              constructor
            )
          )
        case None =>
          // At this point, we cannot tell whether the symbol is not defined within the type at all,
          // or whether it is an instance method.
          Right(
            IR.Error.ImportExport.NoSuchConstructor(
              typeName.get,
              symbolToImport.name
            )
          )
      }
    } else {
      // We want to import only `symbolToImport` from `module`
      bindingMap.definedEntities.find(_.name == symbolToImport.name) match {
        case Some(definedEntity) =>
          definedEntity match {
            case tp: BindingsMap.Type =>
              logger.log(
                Level.FINER,
                "Resolved type {0} from definedEntities in module {1}",
                Array[Object](tp, module)
              )
              Left(
                BindingsMap.ResolvedType(ModuleReference.Concrete(module), tp)
              )
            case method: BindingsMap.ModuleMethod =>
              Left(
                BindingsMap.ResolvedMethod(
                  ModuleReference.Concrete(module),
                  method
                )
              )
            case BindingsMap.PolyglotSymbol(_) =>
              // TODO: Resolve to a polyglot symbol, not to a module
              Left(
                BindingsMap.ResolvedModule(
                  ModuleReference.Concrete(module)
                )
              )
          }
        case None =>
          Right(
            IR.Error.ImportExport.SymbolsDoNotExist(
              List(symbolToImport.name),
              module.getName.toString
            )
          )
      }
    }
  }

  /**
   * Tries to resolve a symbol (of a type or a module) from exported symbols within
   * the given `bindingMap`.
   * @param bindingMap
   * @param module A BindingMap of this module is searched for the symbols
   * @param symbolToImport Symbol to import from a type or from a module
   * @param typeName If defined, a member from a type is imported, rather than a member of a module.
   * @return Either ImportTarget in case of successful resolution, or an import error reason IR.
   */
  private def tryResolveFromExportedSymbols(
    module: Module,
    symbolToImport: IR.Name.Literal,
    typeName: Option[String]
  ): Either[ImportTarget, IR.Error.ImportExport.Reason] = {
    logger.log(
      Level.FINEST,
      "Trying to resolve symbol {0} from type {1} from module {2} exported symbols",
      Array[Object](symbolToImport, typeName, module.getName)
    )
    val bindingMap = module.getIr.unsafeGetMetadata(
      BindingAnalysis, "Should be here"
    )
    if (typeName.isDefined) {
      bindingMap.exportedSymbols.get(typeName.get) match {
        case Some(foundNamesForTypeName) =>
          foundNamesForTypeName.collectFirst { case x: BindingsMap.ResolvedType => x } match {
            case Some(resolvedType) =>
              // Find `symbolToImport` member from `resolvedType`
              val foundConstructor = resolvedType.tp.members.find(_.name == symbolToImport.name)
              foundConstructor match {
                case Some(ctor) => Left(BindingsMap.ResolvedConstructor(resolvedType, ctor))
                case None =>
                  Right(
                    IR.Error.ImportExport.NoSuchConstructor(
                      resolvedType.tp.name,
                      symbolToImport.name
                    )
                  )
              }
            case None =>
              Right(
                IR.Error.ImportExport.TypeDoesNotExist(
                  symbolToImport.name,
                  module.getName.toString
                )
              )
          }
        case None =>
          Right(
            IR.Error.ImportExport.TypeDoesNotExist(
              symbolToImport.name,
              module.getName.toString
            )
          )
      }
    } else {
      bindingMap.exportedSymbols.get(symbolToImport.name) match {
        case Some(resolvedNames) =>
          val resolvedImportTargets: List[ImportTarget] =
            resolvedNames.collect {
              case resolvedModule: BindingsMap.ResolvedModule => resolvedModule
              case resolvedType: BindingsMap.ResolvedType => resolvedType
              case resolvedMethod: BindingsMap.ResolvedMethod => resolvedMethod
              case resolvedCtor: BindingsMap.ResolvedConstructor => resolvedCtor
            }
          // Take the first resolvedName that is also ImportTarget
          resolvedImportTargets.headOption match {
            case Some(resolvedImportTarget) => Left(resolvedImportTarget)
            case None => Right(
              IR.Error.ImportExport.SymbolsDoNotExist(
                List(symbolToImport.name),
                module.getName.toString
              )
            )
          }
        case None =>
          Right(
            IR.Error.ImportExport.SymbolsDoNotExist(
              List(symbolToImport.name),
              module.getName.toString
            )
          )
      }
    }
  }

  private def typeExists(
    typeName: String,
    importedBindingMap: BindingsMap
  ): Boolean = {
    importedBindingMap.definedEntities.exists(entity => {
      entity match {
        case BindingsMap.Type(name, _, _) if name == typeName => true
        case _ => false
      }
    })
  }

  private case class SymbolsResolution(
    resolved: List[ImportTarget],
    errorReason: Option[IR.Error.ImportExport.Reason]
  )
}
