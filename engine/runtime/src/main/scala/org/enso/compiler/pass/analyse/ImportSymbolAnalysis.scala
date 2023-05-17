package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.GenerateMethodBodies

import scala.annotation.unused

case object ImportSymbolAnalysis extends IRPass {

  override type Metadata = BindingsMap

  override type Config = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] =
    Seq(BindingAnalysis, GenerateMethodBodies)

  override val invalidatedPasses: Seq[IRPass] =
    Seq()

  /**
   * @inheritdoc
   */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    val bindingMap = ir.unsafeGetMetadata(BindingAnalysis, "BindingMap should already be present")
    ir.copy(
      imports =
        ir.imports.map(analyseSymbolsFromImport(_, bindingMap))
    )
  }

  /**
   * @inheritdoc
   */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = ir

  private def analyseSymbolsFromImport(
    imp: IR.Module.Scope.Import,
    bindingMap: BindingsMap
  ): IR.Module.Scope.Import = {
    imp match {
      case imp@IR.Module.Scope.Import.Module(
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
        val importedTarget = bindingMap
          .resolvedImports
          .find(_.importDef == imp)
          .getOrElse(throw new CompilerError("Imported module not found"))
          .target
        val maybeFoundSymbols = tryFindSymbols(importedTarget, onlyNames)
        if (maybeFoundSymbols.exists(_.isLeft)) {
          val notFoundSymbols: List[String] = maybeFoundSymbols
            .collect { case Left(symbol) => symbol }
          importedTarget match {
            case BindingsMap.ResolvedModule(module) =>
              IR.Error.ImportExport(
                imp,
                IR.Error.ImportExport.SymbolsDoNotExist(
                  notFoundSymbols,
                  module.getName.toString
                )
              )
            case BindingsMap.ResolvedType(_, tp) =>
              IR.Error.ImportExport(
                imp,
                IR.Error.ImportExport.NoSuchConstructors(
                  tp.name,
                  notFoundSymbols,
                )
              )
          }
        } else {
          imp
        }
      case _ => imp
    }
  }

  private def tryFindSymbols(
    importTarget: BindingsMap.ImportTarget,
    symbols: List[IR.Name.Literal]
  ): List[Either[String, Boolean]] = {
    symbols.map(symbol => {
      val exportedSymbolFromImportedModule =
        importTarget.findExportedSymbolsFor(symbol.name)
      if (exportedSymbolFromImportedModule.isEmpty) {
        Left(symbol.name)
      } else {
        Right(true)
      }
    })
  }

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr
}
