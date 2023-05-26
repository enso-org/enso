package org.enso.compiler.pass.analyse

import org.enso.interpreter.runtime.Module
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Import
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.IRPass

import scala.collection.mutable

/** A pass that checks for ambiguous (duplicated) symbols from imports. For every encountered
  * imported symbol that has already been imported before, the pass replaces the import with
  * an [[IR.Error.ImportExport]]. Moreover, the original import is saved in the error so that
  * the user can see from which location the symbol was originally imported.
  *
  * Also iterates polyglot imports.
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
    val bindingMap = ir.unsafeGetMetadata(
      BindingAnalysis,
      "BindingMap should already be present"
    )
    val encounteredSymbols = new EncounteredSymbols()
    ir.copy(
      imports = ir.imports.flatMap(
        analyseAmbiguousSymbols(
          _,
          moduleContext.module,
          bindingMap,
          encounteredSymbols
        )
      )
    )
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
            _,
            _,
            _
          ) =>
        onlyNames.flatMap(symbol => {
          tryAddEncounteredSymbol(
            module,
            encounteredSymbols,
            moduleImport,
            symbol.name
          ) match {
            case Some(irError) => List(irError)
            case None          => List(moduleImport)
          }
        })

      // Import all symbols
      case moduleImport @ IR.Module.Scope.Import.Module(
            _,
            _,
            true,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        bindingMap.resolvedImports.find(_.importDef == moduleImport) match {
          case Some(resolvedImport) =>
            val symbolsFromImport =
              resolvedImport.target.exportedSymbols.keySet
            val encounteredErrors: List[IR.Error.ImportExport] =
              symbolsFromImport.toList.flatMap(symbol => {
                tryAddEncounteredSymbol(
                  module,
                  encounteredSymbols,
                  moduleImport,
                  symbol
                )
              })
            if (encounteredErrors.nonEmpty) {
              encounteredErrors
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
            _,
            _,
            _
          ) =>
        tryAddEncounteredSymbol(
          module,
          encounteredSymbols,
          moduleImport,
          rename.name
        ) match {
          case Some(irError) => List(irError)
          case None          => List(moduleImport)
        }

      // Import one symbol
      case moduleImport @ IR.Module.Scope.Import.Module(
            symbolName,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        tryAddEncounteredSymbol(
          module,
          encounteredSymbols,
          moduleImport,
          symbolName.parts.last.name
        ) match {
          case Some(irError) => List(irError)
          case None          => List(moduleImport)
        }

      case polyImport @ Import.Polyglot(_, rename, _, _, _) =>
        val symbolName = rename match {
          case Some(rename) => rename
          case None         => polyImport.entity.getVisibleName
        }
        tryAddEncounteredSymbol(
          module,
          encounteredSymbols,
          polyImport,
          symbolName
        ) match {
          case Some(irError) => List(irError)
          case None          => List(polyImport)
        }

      case _ => List(imp)
    }
  }

  private def tryAddEncounteredSymbol(
    module: Module,
    encounteredSymbols: EncounteredSymbols,
    currentImport: IR.Module.Scope.Import,
    symbol: String
  ): Option[IR.Error.ImportExport] = {
    encounteredSymbols.getOriginalImportForSymbol(symbol) match {
      case Some(definingImport) =>
        Some(
          createErrorForAmbiguousImport(
            module,
            definingImport,
            currentImport,
            symbol
          )
        )
      case None =>
        encounteredSymbols.addSymbol(currentImport, symbol)
        None
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

  private class EncounteredSymbols(
    private val encounteredSymbols: mutable.Map[
      String,
      IR.Module.Scope.Import
    ] = mutable.HashMap.empty
  ) {
    def addSymbol(
      imp: IR.Module.Scope.Import,
      symbol: String
    ): Unit = {
      encounteredSymbols.put(symbol, imp)
    }

    /** Returns the original import from which the symbol was imported, if any.
      */
    def getOriginalImportForSymbol(
      symbol: String
    ): Option[IR.Module.Scope.Import] = {
      encounteredSymbols.get(symbol)
    }
  }
}
