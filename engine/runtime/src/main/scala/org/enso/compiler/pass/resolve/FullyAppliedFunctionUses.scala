package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{Expression, Module}
import org.enso.compiler.core.ir.Name
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{Resolution, ResolvedConstructor}
import org.enso.compiler.pass.IRPass

/** Resolves parameter-less calls to Atom Constructors with the parameter list
  * fully defaulted.
  */
object FullyAppliedFunctionUses extends IRPass {

  override type Metadata = BindingsMap.Resolution
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] =
    Seq(GlobalNames)
  override val invalidatedPasses: Seq[IRPass] = Seq()

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
    ir: Module,
    moduleContext: ModuleContext
  ): Module = ir.mapExpressions(doExpression)

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
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = doExpression(ir)

  private def doExpression(expr: Expression): Expression = {
    expr.transformExpressions {
      case app: Application.Prefix =>
        app.copy(arguments = app.arguments.map(_.mapExpressions(doExpression)))
      case name: Name.Literal =>
        val meta = name.getMetadata(GlobalNames)
        meta match {
          case Some(Resolution(ResolvedConstructor(_, cons)))
              if cons.allFieldsDefaulted && cons.arity > 0 =>
            Application.Prefix(name, List(), false, None);
          case _ => name
        }
    }
  }
}
