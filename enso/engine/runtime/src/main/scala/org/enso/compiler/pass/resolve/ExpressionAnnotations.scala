package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.compiler.pass.resolve.ModuleAnnotations.Annotations

import scala.annotation.unused

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
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
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
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = {
    doExpression(ir)
  }

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr

  private def doExpression(
    ir: IR.Expression
  ): IR.Expression =
    ir.transformExpressions {
      case app @ IR.Application.Prefix(
            ann: IR.Name.Annotation,
            arguments,
            _,
            _,
            _,
            _
          ) =>
        if (isKnownAnnotation(ann.name)) {
          arguments match {
            case List() =>
              throw new CompilerError(
                "Impossible, application with no arguments."
              )
            case List(arg) =>
              doExpression(arg.value)
                .updateMetadata(this -->> Annotations(Seq(ann)))
            case realFun :: args =>
              val recurFun = doExpression(realFun.value)
              val (finalFun, preArgs) = recurFun match {
                case IR.Application.Prefix(nextFun, moreArgs, _, _, _, _) =>
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
            IR.Error.Resolution(ann, IR.Error.Resolution.UnknownAnnotation)
          app.copy(function = err)
        }
      case ann: IR.Name.Annotation =>
        if (isKnownAnnotation(ann.name)) {
          IR.Error.Resolution(
            ann,
            IR.Error.Resolution.UnexpectedAnnotation
          )
        } else {
          IR.Error.Resolution(ann, IR.Error.Resolution.UnknownAnnotation)
        }
    }

  /** Checks if `name` is a known annotation.
    *
    * @param name the annotation name to check
    * @return `true` if `name` is a known annotation, otherwise `false`
    */
  def isKnownAnnotation(name: String): Boolean = {
    knownAnnotations.contains(name)
  }
}
