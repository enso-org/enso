package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{Resolution, ResolvedModule}
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.BindingAnalysis

/** Resolves calls to methods defined on modules, called on direct (resolved)
  * module references.
  */
object MethodCalls extends IRPass {

  override type Metadata = BindingsMap.Resolution
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] =
    Seq(BindingAnalysis)
  override val invalidatedPasses: Seq[IRPass] = Seq()

  override def updateMetadataInDuplicate[T <: IR](sourceIr: T, copyOfIr: T): T =
    copyOfIr

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir`.
    *
    * @param ir            the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    ir.mapExpressions(
      doExpression(ir.unsafeGetMetadata(BindingAnalysis, ""), _)
    )
  }

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir` in an inline context.
    *
    * @param ir            the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = {
    doExpression(
      inlineContext.module.getIr.unsafeGetMetadata(BindingAnalysis, ""),
      ir
    )
  }

  private def doExpression(
    bindingsMap: BindingsMap,
    expr: IR.Expression
  ): IR.Expression = {
    expr.transformExpressions { case app: IR.Application.Prefix =>
      def fallback = app.mapExpressions(doExpression(bindingsMap, _))
      app.function match {
        case name: IR.Name if name.isMethod =>
          app.arguments match {
            case first :: _ =>
              val targetBindings =
                first.value.getMetadata(GlobalNames) match {
                  case Some(Resolution(ResolvedModule(module))) =>
                    val moduleIr = module.unsafeAsModule().getIr
                    Option
                      .when(moduleIr != null)(
                        moduleIr.getMetadata(BindingAnalysis)
                      )
                      .flatten
                  case _ => None
                }
              targetBindings match {
                case Some(bindings) =>
                  val resolution =
                    bindings.exportedSymbols.get(name.name)
                  resolution match {
                    case Some(List(resolution)) =>
                      val newName =
                        name.updateMetadata(this -->> Resolution(resolution))
                      val newArgs =
                        app.arguments.map(
                          _.mapExpressions(doExpression(bindingsMap, _))
                        )
                      app.copy(function = newName, arguments = newArgs)
                    case _ => fallback
                  }
                case _ => fallback
              }
            case _ => fallback
          }
        case _ => fallback
      }
    }
  }
}
