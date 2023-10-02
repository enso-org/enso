package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{Expression, Module, Name}
import org.enso.compiler.core.ir.module.scope.Import
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.GenerateMethodBodies

/** Performs analysis of `from ... import sym1, sym2, ...` statements - checks that all
  * the symbols imported from the module can be resolved, i.e., exists.
  * In case of unresolved symbols, replaces the IR import with [[errors.ImportExport]].
  * Reports only the first unresolved symbol.
  */
case object ImportSymbolAnalysis extends IRPass {

  override type Metadata = BindingsMap

  override type Config = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRPass] =
    Seq(BindingAnalysis, GenerateMethodBodies)

  override lazy val invalidatedPasses: Seq[IRPass] =
    Seq()

  /** @inheritdoc
    */
  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    val bindingMap = ir.unsafeGetMetadata(
      BindingAnalysis,
      "BindingMap should already be present"
    )
    ir.copy(
      imports = ir.imports.flatMap(analyseSymbolsFromImport(_, bindingMap))
    )
  }

  /** @inheritdoc
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = ir

  /** @return May return multiple [[errors.ImportExport]] in case of multiple unresolved symbols.
    */
  private def analyseSymbolsFromImport(
    imp: Import,
    bindingMap: BindingsMap
  ): List[Import] = {
    imp match {
      case imp @ Import.Module(
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
            val unresolvedSymbols =
              onlyNames.filterNot(isSymbolResolved(importedTarget, _))
            if (unresolvedSymbols.nonEmpty) {
              unresolvedSymbols
                .map(
                  createErrorForUnresolvedSymbol(
                    imp,
                    importedTarget,
                    _
                  )
                )
            } else {
              List(imp)
            }
          case None => List(imp)
        }
      case _ => List(imp)
    }
  }

  private def createErrorForUnresolvedSymbol(
    imp: Import,
    importTarget: BindingsMap.ImportTarget,
    unresolvedSymbol: Name.Literal
  ): errors.ImportExport = {
    importTarget match {
      case BindingsMap.ResolvedModule(module) =>
        errors.ImportExport(
          imp,
          errors.ImportExport.SymbolDoesNotExist(
            unresolvedSymbol.name,
            module.getName.toString
          )
        )
      case BindingsMap.ResolvedType(_, tp) =>
        errors.ImportExport(
          imp,
          errors.ImportExport.NoSuchConstructor(
            tp.name,
            unresolvedSymbol.name
          )
        )
    }
  }

  private def isSymbolResolved(
    importTarget: BindingsMap.ImportTarget,
    symbol: Name.Literal
  ): Boolean = {
    importTarget.findExportedSymbolsFor(symbol.name).nonEmpty
  }
}
