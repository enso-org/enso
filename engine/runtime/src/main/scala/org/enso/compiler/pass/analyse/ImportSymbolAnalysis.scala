package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.GenerateMethodBodies

import scala.annotation.unused

/** Performs analysis of `from ... import sym1, sym2, ...` statements - checks that all
  * the symbols imported from the module can be resolved, i.e., exists.
  * In case of unresolved symbols, replaces the IR import with [[IR.Error.ImportExport]].
  * Reports only the first unresolved symbol.
  */
case object ImportSymbolAnalysis extends IRPass {

  override type Metadata = BindingsMap

  override type Config = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] =
    Seq(BindingAnalysis, GenerateMethodBodies)

  override val invalidatedPasses: Seq[IRPass] =
    Seq()

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
    ir.copy(
      imports = ir.imports.map(analyseSymbolsFromImport(_, bindingMap))
    )
  }

  /** @inheritdoc
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
      case imp @ IR.Module.Scope.Import.Module(
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
        bindingMap.resolvedImports.find(_.importDef == imp) match {
          case Some(resolvedImport) =>
            val importedTarget = resolvedImport.target
            onlyNames.find(!isSymbolResolved(importedTarget, _)) match {
              case Some(unresolvedSymbol) =>
                importedTarget match {
                  case BindingsMap.ResolvedModule(module) =>
                    IR.Error.ImportExport(
                      imp,
                      IR.Error.ImportExport.SymbolDoesNotExist(
                        unresolvedSymbol.name,
                        module.getName.toString
                      )
                    )
                  case BindingsMap.ResolvedType(_, tp) =>
                    IR.Error.ImportExport(
                      imp,
                      IR.Error.ImportExport.NoSuchConstructor(
                        tp.name,
                        unresolvedSymbol.name
                      )
                    )
                }
              case None => imp
            }
          case None => imp
        }
      case _ => imp
    }
  }

  private def isSymbolResolved(
    importTarget: BindingsMap.ImportTarget,
    symbol: IR.Name.Literal
  ): Boolean = {
    importTarget.findExportedSymbolsFor(symbol.name).nonEmpty
  }

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr
}
