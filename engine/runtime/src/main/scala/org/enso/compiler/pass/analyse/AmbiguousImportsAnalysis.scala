package org.enso.compiler.pass.analyse

import org.enso.interpreter.runtime.Module
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Import
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** A pass that checks for ambiguous and duplicated symbols from imports. For every encountered
  * A duplicated import is an import of a symbol that has already been imported and refers to the
  * same object. On the other hand, an ambiguous import is an import of a symbol that has already
  * been imported but refers to a different object. For every duplicated import, a warning is attached
  * to the IR, and for every ambiguous import, the IR is replaced with an error.
  *
  * One import IR can be replaced with multiple error IRs. This is the case for `from ... import ...`
  * import statements.
  *
  * The original import is saved in the error and warning so that the user can see from which location
  * the symbol was originally imported.
  *
  * Also iterates polyglot imports.
  *
  * All synthetic imports and synthetic modules are ignored by this pass.
  *
  * Thi pass does not alter any metadata.
  */
case object AmbiguousImportsAnalysis extends IRPass {

  override type Metadata = IRPass.Metadata.Empty

  override type Config = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] =
    Seq(BindingAnalysis, ImportSymbolAnalysis)

  override val invalidatedPasses: Seq[IRPass] =
    Seq()

  /** @inheritdoc
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = ir

  /** @inheritdoc
    */
  override def updateMetadataInDuplicate[T <: IR](sourceIr: T, copyOfIr: T): T =
    copyOfIr

  /** @inheritdoc
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    val module = moduleContext.module
    if (module.isSynthetic) {
      ir
    } else {
      val bindingMap = ir.unsafeGetMetadata(
        BindingAnalysis,
        "BindingMap should already be present"
      )
      val encounteredSymbols = new EncounteredSymbols()
      ir.copy(
        imports = ir.imports.flatMap(
          analyseAmbiguousSymbols(
            _,
            module,
            bindingMap,
            encounteredSymbols
          )
        )
      )
    }
  }

  private def analyseAmbiguousSymbols(
    imp: IR.Module.Scope.Import,
    module: Module,
    bindingMap: BindingsMap,
    encounteredSymbols: EncounteredSymbols
  ): List[IR.Module.Scope.Import] = {
    imp match {
      // Import multiple symbols
      case moduleImport @ IR.Module.Scope.Import.Module(
            _,
            _,
            _,
            Some(onlyNames),
            _,
            _,
            false,
            _,
            _
          ) =>
        onlyNames.map(symbol => {
          tryAddEncounteredSymbol(
            module,
            bindingMap,
            encounteredSymbols,
            moduleImport,
            symbol.name
          )
        })

      // Import all symbols
      case moduleImport @ IR.Module.Scope.Import.Module(
            _,
            _,
            true,
            _,
            _,
            _,
            false,
            _,
            _
          ) =>
        getImportTarget(moduleImport, bindingMap) match {
          case Some(importTarget) =>
            val encounteredErrors: ListBuffer[IR.Error.ImportExport] =
              ListBuffer()
            importTarget.exportedSymbols.foreach {
              case (symbolName, resolvedNames) =>
                if (resolvedNames.size > 1) {
                  // Should be handled by other compiler passes
                  throw new CompilerError(
                    s"Unexpected: multiple resolved names for symbol '$symbolName' - should have been handled by previous compiler passes"
                  )
                } else {
                  tryAddEncounteredSymbol(
                    module,
                    bindingMap,
                    encounteredSymbols,
                    moduleImport,
                    symbolName
                  ) match {
                    case irError: IR.Error.ImportExport =>
                      encounteredErrors += irError
                    case _ => ()
                  }
                }
            }
            if (encounteredErrors.nonEmpty) {
              encounteredErrors.toList
            } else {
              List(moduleImport)
            }

          case None =>
            List(moduleImport)
        }

      // Import a renamed symbol
      case moduleImport @ IR.Module.Scope.Import.Module(
            _,
            Some(rename),
            _,
            _,
            _,
            _,
            false,
            _,
            _
          ) =>
        List(
          tryAddEncounteredSymbol(
            module,
            bindingMap,
            encounteredSymbols,
            moduleImport,
            rename.name
          )
        )

      // Import one symbol
      case moduleImport @ IR.Module.Scope.Import.Module(
            symbolName,
            _,
            _,
            _,
            _,
            _,
            false,
            _,
            _
          ) =>
        List(
          tryAddEncounteredSymbol(
            module,
            bindingMap,
            encounteredSymbols,
            moduleImport,
            symbolName.parts.last.name
          )
        )

      // Polyglot import
      case polyImport @ Import.Polyglot(entity, rename, _, _, _) =>
        val symbolName = rename match {
          case Some(rename) => rename
          case None         => entity.getVisibleName
        }
        // Polyglot imports do not have any resolved name associated, so we try to
        // get the original import rather than the resolved name.
        encounteredSymbols.getOriginalImportForSymbol(symbolName) match {
          case Some(originalImport) =>
            originalImport match {
              case IR.Module.Scope.Import.Polyglot(
                    originalEntity,
                    _,
                    _,
                    _,
                    _
                  ) =>
                if (entity.getVisibleName == originalEntity.getVisibleName) {
                  // Same symbol, same entity --> duplicated import --> attach duplicated import warning
                  val warn =
                    createWarningForDuplicatedImport(
                      module,
                      originalImport,
                      polyImport,
                      symbolName
                    )
                  List(polyImport.addDiagnostic(warn))
                } else {
                  // Same symbol, different entity --> ambiguous import --> generate error
                  List(
                    createErrorForAmbiguousImport(
                      module,
                      originalImport,
                      polyImport,
                      symbolName
                    )
                  )
                }
              case _ =>
                // originalImport is a different type than Import.Polyglot, which means that
                // symbolName already points to a different object than a polyglot entity.
                // This is an error.
                List(
                  createErrorForAmbiguousImport(
                    module,
                    originalImport,
                    polyImport,
                    symbolName
                  )
                )
            }
          case None =>
            encounteredSymbols.addSymbol(polyImport, symbolName, None)
            List(polyImport)
        }

      case _ => List(imp)
    }
  }

  private def getImportTarget(
    imp: IR.Module.Scope.Import,
    bindingMap: BindingsMap
  ): Option[BindingsMap.ImportTarget] = {
    bindingMap.resolvedImports.find(_.importDef == imp) match {
      case Some(resolvedImport) =>
        Some(resolvedImport.target)
      case None =>
        None
    }
  }

  /** Tries to add the encountered symbol to the encountered symbols map. If it is already contained
    * in the map, checks whether the underlying resolved name is the same as the original resolved name.
    * Based on that, either attaches a warning for a duplicated import, or returns an IR.
    *
    * Does not handle polyglot imports.
    *
    * @param module Current module
    * @param bindingMap Bindings map for the current module
    * @param encounteredSymbols Encountered symbols in the current module
    * @param currentImport Currently iterated import
    * @param symbolName Name of the symbol that is about to be processed
    * @return
    */
  private def tryAddEncounteredSymbol(
    module: Module,
    bindingMap: BindingsMap,
    encounteredSymbols: EncounteredSymbols,
    currentImport: IR.Module.Scope.Import,
    symbolName: String
  ): IR.Module.Scope.Import = {
    getImportTarget(currentImport, bindingMap) match {
      case Some(resolvedName) =>
        encounteredSymbols.getResolvedNameForSymbol(symbolName) match {
          case Some(encounteredResolvedName) =>
            if (encounteredResolvedName == resolvedName) {
              // It is a duplicate --> create a warning
              val warn =
                createWarningForDuplicatedImport(
                  module,
                  encounteredSymbols.getOriginalImportForSymbol(symbolName).get,
                  currentImport,
                  symbolName
                )
              currentImport.addDiagnostic(warn)
            } else {
              createErrorForAmbiguousImport(
                module,
                encounteredSymbols.getOriginalImportForSymbol(symbolName).get,
                currentImport,
                symbolName
              )
            }
          case None =>
            encounteredSymbols.addSymbol(
              currentImport,
              symbolName,
              Some(resolvedName)
            )
            currentImport
        }

      case None =>
        encounteredSymbols.addSymbol(currentImport, symbolName, None)
        currentImport
    }
  }

  private def createErrorForAmbiguousImport(
    module: Module,
    originalImport: IR.Module.Scope.Import,
    duplicatingImport: IR.Module.Scope.Import,
    ambiguousSymbol: String
  ): IR.Error.ImportExport = {
    IR.Error.ImportExport(
      duplicatingImport,
      IR.Error.ImportExport.AmbiguousImport(
        originalImport,
        ambiguousSymbol,
        module.getSource
      )
    )
  }

  private def createWarningForDuplicatedImport(
    module: Module,
    originalImport: IR.Module.Scope.Import,
    duplicatingImport: IR.Module.Scope.Import,
    duplicatedSymbol: String
  ): IR.Warning = {
    IR.Warning.DuplicatedImport(
      duplicatingImport.location,
      originalImport,
      duplicatedSymbol,
      module.getSource
    )
  }

  /** For every encountered symbol name, we keep track of the original import from which it was imported,
    * along with the resolved name (if any). For example, for symbols imported via `polyglot java import`,
    * there is no resolved name.
    */
  private class EncounteredSymbols(
    private val encounteredSymbols: mutable.Map[
      String,
      (IR.Module.Scope.Import, Option[BindingsMap.ResolvedName])
    ] = mutable.HashMap.empty
  ) {

    /** @param imp Import where the symbol is located
      */
    def addSymbol(
      imp: IR.Module.Scope.Import,
      symbol: String,
      resolvedName: Option[BindingsMap.ResolvedName]
    ): Unit = {
      encounteredSymbols.put(symbol, (imp, resolvedName))
    }

    def getResolvedNameForSymbol(
      symbol: String
    ): Option[BindingsMap.ResolvedName] = {
      encounteredSymbols.get(symbol) match {
        case Some((_, resolvedName)) =>
          resolvedName
        case None =>
          None
      }
    }

    /** Returns the original import from which the symbol was imported, if any.
      */
    def getOriginalImportForSymbol(
      symbol: String
    ): Option[IR.Module.Scope.Import] = {
      encounteredSymbols.get(symbol) match {
        case Some((originalImport, _)) => Some(originalImport)
        case None                      => None
      }
    }
  }
}
