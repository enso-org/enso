package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{Expression, Module, Warning}
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.module.scope.Import
import org.enso.compiler.core.ir.module.scope.imports
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** A pass that checks for ambiguous and duplicated symbols from imports.
  * A duplicated import is an import of a symbol that has already been imported and refers to the
  * same object (entity). On the other hand, an ambiguous import is an import of a symbol that has already
  * been imported but refers to a different object. For every duplicated import, a warning is attached
  * to the IR, and for every ambiguous import, the IR is replaced with an error.
  * To identify an object, this pass uses physical path of the object instead of the object itself.
  *
  * One import IR can be replaced with multiple error IRs. This is the case for `from ... import ...`
  * import statements.
  *
  * The original import is saved in the error and warning so that the user can see from which location
  * the symbol was originally imported.
  *
  * Also iterates polyglot imports.
  *
  * All synthetic imports and exports, as well as synthetic modules are ignored by this pass.
  *
  * This pass does not alter any metadata.
  */
case object AmbiguousImportsAnalysis extends IRPass {

  override type Metadata = IRPass.Metadata.Empty

  override type Config = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRPass] =
    Seq(BindingAnalysis, ImportSymbolAnalysis)

  override lazy val invalidatedPasses: Seq[IRPass] =
    Seq()

  /** @inheritdoc
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = ir

  /** @inheritdoc
    */
  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    if (moduleContext.isSynthetic()) {
      ir
    } else {
      val bindingMap = ir.unsafeGetMetadata(
        BindingAnalysis,
        "BindingMap should already be present"
      )
      val encounteredSymbols = new EncounteredSymbols()
      ir.copy(
        imports = ir.imports.flatMap(imp => {
          analyseAmbiguousSymbols(
            imp,
            bindingMap,
            encounteredSymbols
          ).fold(identity, imp => List(imp))
        })
      )
    }
  }

  /** Analyses ambiguous symbols in the given import.
    * @param imp current import
    * @param module current module
    * @param bindingMap binding map of the current module
    * @return A list of errors, if any encountered, or an import IR, potentially with some
    *         warnings attached.
    */
  private def analyseAmbiguousSymbols(
    imp: Import,
    bindingMap: BindingsMap,
    encounteredSymbols: EncounteredSymbols
  ): Either[List[errors.ImportExport], Import] = {
    imp match {
      // Import multiple symbols
      case moduleImport @ Import.Module(
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
        getImportTarget(moduleImport, bindingMap) match {
          case Some(importTarget) =>
            val encounteredErrors: ListBuffer[errors.ImportExport] =
              ListBuffer()
            val imp =
              onlyNames.foldLeft(moduleImport: Import) { case (imp, symbol) =>
                val symbolName = symbol.name
                importTarget.resolveExportedSymbol(symbolName) match {
                  case Right(resolvedName) =>
                    val symbolPath = resolvedName.qualifiedName.toString
                    tryAddEncounteredSymbol(
                      encounteredSymbols,
                      imp,
                      symbolName,
                      symbolPath
                    ) match {
                      case Left(error) =>
                        encounteredErrors += error
                        imp
                      case Right(imp) => imp
                    }
                  case Left(resolutionError) =>
                    throw new CompilerError(
                      s"Unreachable: (should have been resolved in previous passes) $resolutionError"
                    )
                }
              }
            if (encounteredErrors.nonEmpty) {
              Left(encounteredErrors.toList)
            } else {
              Right(imp)
            }

          case None =>
            Right(moduleImport)
        }

      // Import all symbols
      case moduleImport @ Import.Module(
            _,
            _,
            true,
            _,
            hiddenNames,
            _,
            false,
            _,
            _
          ) =>
        getImportTarget(moduleImport, bindingMap) match {
          case Some(importTarget) =>
            // Names of the symbols that are exported by a module or a type referred to via importTarget
            val exportedSymbolNames: List[String] =
              importTarget.exportedSymbols.keySet.toList
            val symbolsToIterate = hiddenNames match {
              case None => exportedSymbolNames
              case Some(hiddenNamesLiterals) =>
                val hiddenNames = hiddenNamesLiterals.map(_.name)
                exportedSymbolNames.filterNot(hiddenNames.contains)
            }
            val encounteredErrors: ListBuffer[errors.ImportExport] =
              ListBuffer()
            val imp =
              symbolsToIterate.foldLeft(moduleImport: Import) {
                case (imp, symbolName) =>
                  importTarget.resolveExportedSymbol(symbolName) match {
                    case Left(resolutionError) =>
                      throw new CompilerError(
                        s"Unreachable: (should have been resolved in previous passes) $resolutionError"
                      )
                    case Right(resolvedName) =>
                      tryAddEncounteredSymbol(
                        encounteredSymbols,
                        imp,
                        symbolName,
                        resolvedName.qualifiedName.toString
                      ) match {
                        case Left(error) =>
                          encounteredErrors += error
                          imp
                        case Right(imp) => imp
                      }
                  }
              }
            if (encounteredErrors.nonEmpty) {
              Left(encounteredErrors.toList)
            } else {
              Right(imp)
            }

          case None =>
            Right(moduleImport)
        }

      // Import a renamed symbol
      case moduleImport @ Import.Module(
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
        tryAddEncounteredSymbol(
          encounteredSymbols,
          moduleImport,
          rename.name,
          symbolPath
        ) match {
          case Left(error) => Left(List(error))
          case Right(imp)  => Right(imp)
        }

      // Import one symbol
      case moduleImport @ Import.Module(
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
        tryAddEncounteredSymbol(
          encounteredSymbols,
          moduleImport,
          importPath.parts.last.name,
          importPath.name
        ) match {
          case Left(err)  => Left(List(err))
          case Right(imp) => Right(imp)
        }

      // Polyglot import
      case polyImport @ imports.Polyglot(entity, rename, _, _, _) =>
        val symbolName = rename.getOrElse(entity.getVisibleName)
        val symbolPath = entity match {
          case imports.Polyglot.Java(packageName, className) =>
            packageName + "." + className
        }
        tryAddEncounteredSymbol(
          encounteredSymbols,
          polyImport,
          symbolName,
          symbolPath
        ) match {
          case Left(err)  => Left(List(err))
          case Right(imp) => Right(imp)
        }

      case _ => Right(imp)
    }
  }

  private def getImportTarget(
    imp: Import,
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
    * in the map, checks whether the underlying entity path is the same as the original entity path.
    * Based on that, either attaches a warning for a duplicated import, or returns an [[errors.ImportExport]].
    *
    * @param encounteredSymbols Encountered symbols in the current module
    * @param currentImport Currently iterated import
    * @param symbolName Name of the symbol that is about to be processed
    * @param symbolPath physical path of the symbol that is about to be processed
    * @return
    */
  private def tryAddEncounteredSymbol(
    encounteredSymbols: EncounteredSymbols,
    currentImport: Import,
    symbolName: String,
    symbolPath: String
  ): Either[errors.ImportExport, Import] = {
    if (encounteredSymbols.containsSymbol(symbolName)) {
      val encounteredFullName =
        encounteredSymbols.getPathForSymbol(symbolName)
      val originalImport =
        encounteredSymbols.getOriginalImportForSymbol(symbolName).get
      if (symbolPath == encounteredFullName) {
        val warn =
          createWarningForDuplicatedImport(
            originalImport,
            currentImport,
            symbolName
          )
        Right(currentImport.addDiagnostic(warn))
      } else {
        Left(
          createErrorForAmbiguousImport(
            originalImport,
            encounteredFullName,
            currentImport,
            symbolName,
            symbolPath
          )
        )
      }
    } else {
      encounteredSymbols.addSymbol(currentImport, symbolName, symbolPath)
      Right(currentImport)
    }
  }

  private def createErrorForAmbiguousImport(
    originalImport: Import,
    originalSymbolPath: String,
    duplicatingImport: Import,
    ambiguousSymbol: String,
    ambiguousSymbolPath: String
  ): errors.ImportExport = {
    errors.ImportExport(
      duplicatingImport,
      errors.ImportExport.AmbiguousImport(
        originalImport,
        originalSymbolPath,
        ambiguousSymbol,
        ambiguousSymbolPath
      )
    )
  }

  private def createWarningForDuplicatedImport(
    originalImport: Import,
    duplicatingImport: Import,
    duplicatedSymbol: String
  ): Warning = {
    Warning.DuplicatedImport(
      duplicatingImport.location,
      originalImport,
      duplicatedSymbol
    )
  }

  /** For every encountered symbol name, we keep track of the original import from which it was imported,
    * along with the entity path. The entity path is vital to decide whether an imported symbol is duplicated
    * or ambiguous.
    */
  private class EncounteredSymbols(
    private val encounteredSymbols: mutable.Map[
      String,
      (Import, String)
    ] = mutable.HashMap.empty
  ) {

    def containsSymbol(symbolName: String): Boolean = {
      encounteredSymbols.contains(symbolName)
    }

    /** @param imp Import where the symbol is located
      */
    def addSymbol(
      imp: Import,
      symbol: String,
      symbolPath: String
    ): Unit = {
      encounteredSymbols.put(symbol, (imp, symbolPath))
    }

    /** Returns the entity path for the symbol.
      */
    def getPathForSymbol(
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
    ): Option[Import] = {
      encounteredSymbols.get(symbol).map(_._1)
    }
  }
}
