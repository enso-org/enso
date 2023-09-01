package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{Expression, Module}
import org.enso.compiler.core.ir.Name
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.compiler.pass.resolve.ModuleAnnotations.Annotations

case object ExpressionAnnotations extends IRPass {
  val tailCallName      = "@Tail_Call"
  val builtinMethodName = "@Builtin_Method"
  val autoParallelName  = "@Auto_Parallel"
  val knownAnnotations  = Seq(tailCallName, builtinMethodName, autoParallelName)

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = Annotations

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] = Seq(ModuleAnnotations)

  /** The passes that are invalidated by running this pass. */
  override val invalidatedPasses: Seq[IRPass] = Seq(AliasAnalysis)

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
  ): Module = {
    ir.mapExpressions(doExpression)
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
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
    doExpression(ir)
  }

  /** @inheritdoc */

  private def doExpression(
    ir: Expression
  ): Expression =
    ir.transformExpressions {
      case app @ Application.Prefix(
            ann: Name.BuiltinAnnotation,
            arguments,
            _,
            _,
            _,
            _
          ) =>
        if (isKnownAnnotation(ann.name)) {
          arguments match {
            case List() =>
              errors.Resolution(
                ann,
                errors.Resolution.UnexpectedAnnotation
              )
            case List(arg) =>
              doExpression(arg.value)
                .updateMetadata(this -->> Annotations(Seq(ann)))
            case realFun :: args =>
              val recurFun = doExpression(realFun.value)
              val (finalFun, preArgs) = recurFun match {
                case Application.Prefix(nextFun, moreArgs, _, _, _, _) =>
                  (nextFun, moreArgs)
                case _ => (recurFun, List())
              }
              val recurArgs = args.map(_.mapExpressions(doExpression))
              app
                .copy(function = finalFun, arguments = preArgs ++ recurArgs)
                .updateMetadata(this -->> Annotations(Seq(ann)))
          }
        } else {
          val err =
            errors.Resolution(ann, errors.Resolution.UnknownAnnotation)
          app.copy(function = err)
        }
      case ann: Name.BuiltinAnnotation =>
        if (isKnownAnnotation(ann.name)) {
          errors.Resolution(
            ann,
            errors.Resolution.UnexpectedAnnotation
          )
        } else {
          errors.Resolution(ann, errors.Resolution.UnknownAnnotation)
        }
    }

  /** Checks if `name` is a known annotation.
    *
    * @param name the annotation name to check
    * @return `true` if `name` is a known annotation, otherwise `false`
    */
  private def isKnownAnnotation(name: String): Boolean = {
    knownAnnotations.contains(name)
  }
}
