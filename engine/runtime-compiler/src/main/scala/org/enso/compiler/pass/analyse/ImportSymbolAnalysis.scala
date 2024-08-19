package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
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
          ) =>
        bindingMap.resolvedImports.find(_.importDef == imp) match {
          case Some(resolvedImport) =>
            val importedTargets = resolvedImport.targets
            val unresolvedSymbols = importedTargets.flatMap { importedTarget =>
              onlyNames.filterNot(isSymbolResolved(importedTarget, _))
            }
            if (unresolvedSymbols.nonEmpty) {
              unresolvedSymbols
                .map(
                  createErrorForUnresolvedSymbol(
                    imp,
                    importedTargets.head,
                    _
                  )
                )
            } else {
              List(imp)
            }
          case None => List(imp)
        }
      // Importing symbols from methods is not allowed. The following code checks that if the
      // import is importing all from a method, an error is reported.
      case imp @ Import.Module(
            _,
            _,
            isAll,
            _,
            _,
            _,
            isSynthetic,
            _,
          ) if isAll && !isSynthetic =>
        bindingMap.resolvedImports.find(_.importDef == imp) match {
          case Some(resolvedImport) =>
            val importedTargets = resolvedImport.targets
            val errors = importedTargets.flatMap { importedTarget =>
              importedTarget match {
                case BindingsMap.ResolvedModuleMethod(module, method) =>
                  val err = createImportFromMethodError(
                    imp,
                    module.getName.toString,
                    method.name
                  )
                  Some(err)
                case BindingsMap.ResolvedExtensionMethod(
                      module,
                      staticMethod
                    ) =>
                  val err = createImportFromMethodError(
                    imp,
                    module.getName.createChild(staticMethod.tpName).toString,
                    staticMethod.methodName
                  )
                  Some(err)
                case BindingsMap.ResolvedConversionMethod(
                      module,
                      conversionMethod
                    ) =>
                  val err = createImportFromMethodError(
                    imp,
                    module.getName
                      .createChild(conversionMethod.targetTpName)
                      .toString,
                    conversionMethod.methodName
                  )
                  Some(err)
                case _ => None
              }
            }
            if (errors.nonEmpty) {
              errors
            } else {
              List(imp)
            }
          case None => List(imp)
        }
      case _ => List(imp)
    }
  }

  private def createImportFromMethodError(
    imp: Import,
    moduleName: String,
    methodName: String
  ): errors.ImportExport = {
    errors.ImportExport(
      imp,
      errors.ImportExport.IllegalImportFromMethod(
        moduleName,
        methodName
      )
    )
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
      case BindingsMap.ResolvedConstructor(_, cons) =>
        errors.ImportExport(
          imp,
          errors.ImportExport.NoSuchConstructor(
            cons.name,
            unresolvedSymbol.name
          )
        )
      case BindingsMap.ResolvedModuleMethod(_, method) =>
        errors.ImportExport(
          imp,
          errors.ImportExport.NoSuchModuleMethod(
            method.name,
            unresolvedSymbol.name
          )
        )
      case BindingsMap.ResolvedExtensionMethod(mod, staticMethod) =>
        errors.ImportExport(
          imp,
          errors.ImportExport.NoSuchStaticMethod(
            moduleName = mod.getName.toString,
            typeName   = staticMethod.tpName,
            methodName = unresolvedSymbol.name
          )
        )
      case BindingsMap.ResolvedConversionMethod(mod, conversionMethod) =>
        errors.ImportExport(
          imp,
          errors.ImportExport.NoSuchConversionMethod(
            moduleName     = mod.getName.toString,
            targetTypeName = conversionMethod.targetTpName,
            sourceTypeName = conversionMethod.sourceTpName
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
