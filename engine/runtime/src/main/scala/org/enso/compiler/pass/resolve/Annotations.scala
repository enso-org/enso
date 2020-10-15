package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass

case object Annotations extends IRPass {
  case object TailCallAnnotated extends IRPass.Metadata {
    override val metadataName: String                 = "TailCallAnnotated"
    override def duplicate(): Option[IRPass.Metadata] = Some(this)
  }

  val tailCallName = "@Tail_Call"

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = TailCallAnnotated.type

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
        if (ann.name == tailCallName) {
          arguments match {
            case List() =>
              throw new CompilerError(
                "Impossible, application with no arguments."
              )
            case List(arg) =>
              doExpression(arg.value)
                .updateMetadata(this -->> TailCallAnnotated)
            case realFun :: args =>
              val recurFun  = doExpression(realFun.value)
              val recurArgs = args.map(_.mapExpressions(doExpression))
              app
                .copy(function = recurFun, arguments = recurArgs)
                .updateMetadata(this -->> TailCallAnnotated)
          }
        } else {
          val err =
            IR.Error.Resolution(ann, IR.Error.Resolution.UnknownAnnotation)
          app.copy(function = err)
        }
      case ann: IR.Name.Annotation =>
        if (ann.name == tailCallName) {
          IR.Error.Resolution(
            ann,
            IR.Error.Resolution.UnexpectedTailCallAnnotation
          )
        } else {
          IR.Error.Resolution(ann, IR.Error.Resolution.UnknownAnnotation)
        }
    }

}
