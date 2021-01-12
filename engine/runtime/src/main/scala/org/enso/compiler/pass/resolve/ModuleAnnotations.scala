package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Name
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.{
  ComplexType,
  FunctionBinding,
  GenerateMethodBodies
}

import scala.annotation.unused

/** A pass responsible for the discovery of module annotations, and for
  * associating them with the corresponding construct.
  */
case object ModuleAnnotations extends IRPass {
  override type Metadata = ModuleAnnotated
  override type Config   = IRPass.Configuration.Default
  override val precursorPasses: Seq[IRPass] = Seq()
  override val invalidatedPasses: Seq[IRPass] = Seq(
    DocumentationComments,
    ComplexType,
    FunctionBinding,
    GenerateMethodBodies
  )

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
    var lastAnnotations:Seq[IR.Name.Annotation] = Seq()
    val newBindings = for (binding <- ir.bindings) yield {
      binding match {
        case ann: Name.Annotation =>
          lastAnnotations :+= ann
          None
        case comment: IR.Comment => Some(comment)
        case entity =>
          val res = Some(
            entity.updateMetadata(this -->> ModuleAnnotated(lastAnnotations))
          )
          lastAnnotations = Seq()
          res
      }
    }
    ir.copy(bindings = newBindings.flatten)
  }

  // TODO [AA] Need to move annotations in the type signatures and complex type passes.
  def resolveModuleDef(
    @unused binding: IR.Module.Scope.Definition
  ): IR.Module.Scope.Definition = {
    ???
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
    @unused inlineContext: InlineContext
  ): IR.Expression = ir

  /** A container for annotations at the module level.
    *
    * @param annotations the initial annotations for the container
    */
  case class ModuleAnnotated(annotations: Seq[IR.Name.Annotation])
      extends IRPass.Metadata {
    override val metadataName: String                 = "ModuleAnnotated"
    override def duplicate(): Option[IRPass.Metadata] = Some(this.copy())

    /** Add an annotation to the annotations container.
      *
      * @param annotation the annotation to add
      * @return `this`, with `annotation` added to it
      */
    def addAnnotation(annotation: IR.Name.Annotation): ModuleAnnotated =
      this.copy(annotations = this.annotations :+ annotation)
  }
}
