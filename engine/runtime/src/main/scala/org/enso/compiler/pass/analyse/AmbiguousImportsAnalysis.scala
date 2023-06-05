package org.enso.compiler.pass.analyse

import org.enso.interpreter.runtime.Module
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Import
import org.enso.compiler.core.IR.Module.Scope.Import.Polyglot
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.IRPass

import scala.annotation.unused
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
            importPath,
            _,
            _,
            Some(onlyNames),
            _,
            _,
            false,
            _,
            _
          ) =>
        println(s"Importing multiple symbols from `${imp.showCode()}`")
        encounteredSymbols.debugPrint("before")
        onlyNames.map(symbolName => {
          val symbolPath =
            importPath.name + "." + symbolName.name
          tryAddEncounteredSymbol(
            module,
            encounteredSymbols,
            moduleImport,
            symbolName.name,
            symbolPath
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
            // Names of the symbols that are exported by a module or a type refered to via importTarget
            val exportedSymbolNames: List[String] =
              importTarget.exportedSymbols.keySet.toList

            val encounteredErrors: ListBuffer[IR.Error.ImportExport] =
              ListBuffer()
            exportedSymbolNames.foreach(symbolName => {
              importTarget.resolveExportedSymbol(symbolName) match {
                case Left(_) =>
                  assert(importTarget.exportedSymbols(symbolName).size > 1)
                  val oneOfOriginalPaths =
                    importTarget
                      .exportedSymbols(symbolName)
                      .head
                      .qualifiedName
                      .toString
                  // importTarget tries to export ambiguous symbols
                  encounteredErrors += createErrorForAmbiguousImport(
                    module,
                    moduleImport,
                    oneOfOriginalPaths,
                    moduleImport,
                    symbolName
                  )
                case Right(resolvedName) =>
                  tryAddEncounteredSymbol(
                    module,
                    encounteredSymbols,
                    moduleImport,
                    symbolName,
                    resolvedName.qualifiedName.toString
                  ) match {
                    case error: IR.Error.ImportExport =>
                      encounteredErrors += error
                    case _ => ()
                  }
              }
            })
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
            importPath,
            Some(rename),
            _,
            _,
            _,
            _,
            false,
            _,
            _
          ) =>
        val symbolPath = importPath.name
        List(
          tryAddEncounteredSymbol(
            module,
            encounteredSymbols,
            moduleImport,
            rename.name,
            symbolPath
          )
        )

      // Import one symbol
      case moduleImport @ IR.Module.Scope.Import.Module(
            importPath,
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
            encounteredSymbols,
            moduleImport,
            importPath.parts.last.name,
            importPath.name
          )
        )

      // Polyglot import
      case polyImport @ Import.Polyglot(entity, rename, _, _, _) =>
        val symbolName = rename match {
          case Some(rename) => rename
          case None         => entity.getVisibleName
        }
        val symbolPath = entity match {
          case Polyglot.Java(packageName, className) =>
            packageName + "." + className
        }
        List(
          tryAddEncounteredSymbol(
            module,
            encounteredSymbols,
            polyImport,
            symbolName,
            symbolPath
          )
        )

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
    * @param encounteredSymbols Encountered symbols in the current module
    * @param currentImport Currently iterated import
    * @param symbolName Name of the symbol that is about to be processed
    * @return
    */
  private def tryAddEncounteredSymbol(
    module: Module,
    encounteredSymbols: EncounteredSymbols,
    currentImport: IR.Module.Scope.Import,
    symbolName: String,
    symbolPath: String
  ): IR.Module.Scope.Import = {
    if (encounteredSymbols.containsSymbol(symbolName)) {
      val encounteredFullName =
        encounteredSymbols.getFullNameForSymbol(symbolName)
      val originalImport =
        encounteredSymbols.getOriginalImportForSymbol(symbolName).get
      if (symbolPath == encounteredFullName) {
        // Duplicate
        val warn =
          createWarningForDuplicatedImport(
            module,
            originalImport,
            currentImport,
            symbolName
          )
        currentImport.addDiagnostic(warn)
      } else {
        // Ambiguous
        createErrorForAmbiguousImport(
          module,
          originalImport,
          encounteredFullName,
          currentImport,
          symbolName
        )
      }
    } else {
      encounteredSymbols.addSymbol(currentImport, symbolName, symbolPath)
      currentImport
    }
  }

  private def createErrorForAmbiguousImport(
    module: Module,
    originalImport: IR.Module.Scope.Import,
    originalSymbolPath: String,
    duplicatingImport: IR.Module.Scope.Import,
    ambiguousSymbol: String
  ): IR.Error.ImportExport = {
    IR.Error.ImportExport(
      duplicatingImport,
      IR.Error.ImportExport.AmbiguousImport(
        originalImport,
        originalSymbolPath,
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
    val encounteredSymbols: mutable.Map[
      String,
      (IR.Module.Scope.Import, String)
    ] = mutable.HashMap.empty
  ) {

    def containsSymbol(symbolName: String): Boolean = {
      encounteredSymbols.contains(symbolName)
    }

    /** @param imp Import where the symbol is located
      */
    def addSymbol(
      imp: IR.Module.Scope.Import,
      symbol: String,
      fullName: String
    ): Unit = {
      encounteredSymbols.put(symbol, (imp, fullName))
    }

    def getFullNameForSymbol(
      symbol: String
    ): String = {
      encounteredSymbols.get(symbol) match {
        case Some((_, fullName)) =>
          fullName
        case None =>
          throw new IllegalStateException("unreachable")
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

    @unused
    def debugPrint(title: String): Unit = {
      println(s"Encountered symbols ($title):")
      encounteredSymbols.foreach { case (symbol, (imp, fullName)) =>
        println(s"  $symbol -> (import=`${imp.showCode()}`, path='$fullName')")
      }
    }
  }
}
