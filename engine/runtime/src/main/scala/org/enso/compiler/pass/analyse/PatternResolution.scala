package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.pass.analyse.BindingResolution.ResolvedConstructor

object PatternResolution extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = MethodDefinitionResolution.Resolution

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] = Seq()

  /** The passes that are invalidated by running this pass. */
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
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    val bindings = ir.unsafeGetMetadata(
      BindingResolution,
      "Binding resolution was not run before pattern resolution"
    )
    ir.mapExpressions(doExpression(_, bindings))
  }

  private def doExpression(
    expr: IR.Expression,
    bindings: BindingResolution.LocalBindings
  ): IR.Expression = {
    expr.transformExpressions {
      case caseExpr: IR.Case.Expr =>
        val newBranches = caseExpr.branches.map { branch =>
          val resolvedPattern = branch.pattern match {
            case consPat: IR.Pattern.Constructor =>
              val resolvedName = consPat.constructor match {
                case lit: IR.Name.Literal =>
                  val resolution = bindings.resolveUppercaseName(lit.name)
                  resolution match {
                    case Left(err) =>
                      IR.Error.Resolution(
                        consPat.constructor,
                        IR.Error.Resolution.Reason(err)
                      )
                    case Right(value) =>
                      lit.updateMetadata(
                        this -->> MethodDefinitionResolution.Resolution(value)
                      )
                  }
                case other => other
              }
              val resolution = resolvedName.getMetadata(this)
              val expectedArity = resolution.map { res =>
                res.target match {
                  case ResolvedConstructor(_, cons)        => cons.arity
                  case BindingResolution.ResolvedModule(_) => 0
                }
              }
              expectedArity match {
                case Some(arity) =>
                  if (consPat.fields.length != arity) {
                    IR.Error.Pattern(
                      consPat,
                      IR.Error.Pattern.WrongArity(
                        consPat.constructor.name,
                        arity,
                        consPat.fields.length
                      )
                    )
                  } else {
                    consPat.copy(constructor = resolvedName)
                  }
                case None => consPat.copy(constructor = resolvedName)
              }
            case other => other
          }
          branch.copy(
            pattern    = resolvedPattern,
            expression = doExpression(branch.expression, bindings)
          )
        }
        caseExpr.copy(branches = newBranches)

    }
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
    val bindings = inlineContext.module.getIr.unsafeGetMetadata(
      BindingResolution,
      "Binding resolution was not run before pattern resolution"
    )
    doExpression(ir, bindings)
  }
}
