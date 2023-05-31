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
    println(s"Analyzing ambiguous symbols in '${imp.showCode()}'")
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
            _,
            _,
            _
          ) =>
        getImportTarget(moduleImport, bindingMap) match {
          case Some(importTarget) =>
            importTarget match {
              case resolvedModule: BindingsMap.ResolvedModule =>
                val encounteredErrors: ListBuffer[IR.Error.ImportExport] =
                  ListBuffer()
                resolvedModule.exportedSymbols.foreach {
                  case (symbolName, resolvedNames) =>
                    if (resolvedNames.size > 1) {
                      // TODO: Report IR.Error.ImportExport ?
                      // Should be handled by other compiler passes
                      throw new CompilerError(
                        s"Ambiguous import of $symbolName"
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
              case BindingsMap.ResolvedType(_, _) =>
                throw new CompilerError("ResolvedType not yet supported")
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
            _,
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

      case polyImport @ Import.Polyglot(entity, rename, _, _, _) =>
        val symbolName = rename match {
          case Some(rename) => rename
          case None         => entity.getVisibleName
        }
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
                  // Same symbol, same entity --> attach duplicated import warning
                  val warn =
                    createWarningForDuplicatedImport(
                      module,
                      originalImport,
                      polyImport,
                      symbolName
                    )
                  List(polyImport.addDiagnostic(warn))
                } else {
                  // Same symbol, different entity --> generate error
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
    println(s"Creating error for ambiguous import of $ambiguousSymbol")
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
    println(s"Creating warning for duplicated import of $duplicatedSymbol")
    IR.Warning.DuplicatedImport(
      duplicatingImport.location,
      originalImport,
      duplicatedSymbol,
      module.getSource
    )
  }

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
      println(s"Adding symbol $symbol to encountered symbols")
      encounteredSymbols.put(symbol, (imp, resolvedName))
      debugPrint()
    }

    def getResolvedNameForSymbol(
      symbol: String
    ): Option[BindingsMap.ResolvedName] = {
      print(s"Getting resolved name for symbol $symbol: ")
      encounteredSymbols.get(symbol) match {
        case Some((_, resolvedName)) =>
          println(s"Found resolved name $resolvedName")
          resolvedName
        case None =>
          println("No resolved name found")
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

    def debugPrint(): Unit = {
      println("Encountered symbols:")
      encounteredSymbols.foreach { case (symbol, (imp, resolvedNameOpt)) =>
        val resolvedNameRepr = resolvedNameOpt match {
          case Some(resolvedName) =>
            resolvedName match {
              case BindingsMap.ResolvedType(_, tp) =>
                s"ResolvedType(${tp.name})"
              case BindingsMap.ResolvedConstructor(_, cons) =>
                s"ResolvedConstructor(${cons.name})"
              case BindingsMap.ResolvedModule(module) =>
                s"ResolvedModule(${module.getName})"
              case BindingsMap.ResolvedMethod(_, method) =>
                s"ResolvedMethod(${method.name})"
              case BindingsMap.ResolvedPolyglotSymbol(module, symbol) =>
                s"ResolvedPolyglotSymbol(${module.getName}, ${symbol})"
            }
          case None => "None"
        }
        val impRepr = "'" + imp.showCode() + "'"
        println(
          s"  $symbol: importDef = $impRepr, resolvedName = $resolvedNameRepr"
        )
      }
    }
  }
}
