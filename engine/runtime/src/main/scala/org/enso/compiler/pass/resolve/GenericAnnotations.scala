package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.core.IR.Name
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.resolve.ModuleAnnotations.Annotations

/** A pass responsible for the discovery of module annotations, and for
  * associating them with the corresponding construct.
  */
case object GenericAnnotations extends IRPass {
  override type Metadata = Annotations
  override type Config   = IRPass.Configuration.Default
  override val precursorPasses: Seq[IRPass]   = Seq()
  override val invalidatedPasses: Seq[IRPass] = Seq()

  /** Resolves module-level annotations.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    *  @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    var lastAnnotations: Seq[IR.Name.GenericAnnotation] = Seq()
    val newBindings = ir.bindings.map {
      case ann: Name.BuiltinAnnotation =>
        throw new CompilerError(
          s"Builtin annotations should not be present at generic annotations pass [${ann.name}]."
        )
      case ann: Name.GenericAnnotation =>
        lastAnnotations :+= ann
        None
      case comment: IR.Comment =>
        Some(comment)
      case typ: Definition.SugaredType =>
        val res = Some(
          resolveComplexType(typ).updateMetadata(
            this -->> Annotations(lastAnnotations)
          )
        )
        lastAnnotations = Seq()
        res
      case entity =>
        val res = Some(
          entity.updateMetadata(this -->> Annotations(lastAnnotations))
        )
        lastAnnotations = Seq()
        res
    }
    ir.copy(bindings = newBindings.flatten)
  }

  /** Resolves top level annotations within a complex type.
    *
    * @param typ the type in which to resolve annotations
    * @return `typ` with all top-level annotations resolved
    */
  private def resolveComplexType(
    typ: Definition.SugaredType
  ): Definition.SugaredType = {
    var lastAnnotations: Seq[IR.Name.GenericAnnotation] = Seq()
    val newBodyElems = typ.body.flatMap {
      case ann: Name.BuiltinAnnotation =>
        throw new CompilerError(
          s"Builtin annotations should not be present at generic annotations pass [${ann.name}]."
        )
      case ann: Name.GenericAnnotation =>
        lastAnnotations :+= ann
        None
      case comment: IR.Comment =>
        Some(comment)
      case entity =>
        val res = Some(
          entity.updateMetadata(this -->> Annotations(lastAnnotations))
        )
        lastAnnotations = Seq()
        res
    }
    typ.copy(body = newBodyElems)
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
