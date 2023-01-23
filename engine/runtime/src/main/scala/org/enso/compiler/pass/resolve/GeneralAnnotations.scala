package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.resolve.ModuleAnnotations.Annotations

/** A pass responsible for the discovery of [[IR.Name.GeneralAnnotation]]
  * annotations, and for associating them with the corresponding construct.
  *
  * Annotations defined on a type constructor are resolved in the
  * [[org.enso.compiler.pass.desugar.ComplexType]] pass.
  */
case object GeneralAnnotations extends IRPass {
  override type Metadata = Annotations
  override type Config   = IRPass.Configuration.Default
  override val precursorPasses: Seq[IRPass]   = Seq()
  override val invalidatedPasses: Seq[IRPass] = Seq()

  /** Resolves annotations.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    * to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    * IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    var lastAnnotations: Seq[IR.Name.GeneralAnnotation] = Seq()
    val newBindings = ir.bindings.map {
      case _: IR.Name.BuiltinAnnotation =>
        throw new CompilerError(
          s"Builtin annotations should not be present at generic annotations pass."
        )
      case _: Definition.SugaredType =>
        throw new CompilerError(
          s"Sugared types should not be present at generic annotations pass."
        )
      case _: IR.Comment =>
        throw new CompilerError(
          "Comments should not be present at generic annotations pass."
        )
      case ann: IR.Name.GeneralAnnotation =>
        lastAnnotations :+= ann
        None
      case entity =>
        val res = Some(
          entity.updateMetadata(this -->> Annotations(lastAnnotations))
        )
        lastAnnotations = Seq()
        res
    }
    ir.copy(bindings = newBindings.flatten)
  }

  /** Execute the pass on an expression.
    *
    * As the pass only deals with module-level annotations this is a no-op.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    *  @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = ir

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr
}
