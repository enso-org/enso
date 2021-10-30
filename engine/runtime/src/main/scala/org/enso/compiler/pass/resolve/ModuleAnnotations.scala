package org.enso.compiler.pass.resolve

import org.enso.compiler.Compiler
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition
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
  override type Metadata = Annotations
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
    var lastAnnotations: Seq[IR.Name.Annotation] = Seq()
    val newBindings = for (binding <- ir.bindings) yield {
      binding match {
        case ann: Name.Annotation =>
          lastAnnotations :+= ann
          None
        case comment: IR.Comment => Some(comment)
        case typ: Definition.Type =>
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
    }
    ir.copy(bindings = newBindings.flatten)
  }

  /** Resolves top level annotations within a complex type.
    *
    * @param typ the type in which to resolve annotations
    * @return `typ` with all top-level annotations resolved
    */
  def resolveComplexType(typ: Definition.Type): Definition.Type = {
    var lastAnnotations: Seq[IR.Name.Annotation] = Seq()
    val newBodyElems = typ.body.flatMap {
      case ann: Name.Annotation =>
        lastAnnotations :+= ann
        None
      case comment: IR.Comment => Some(comment)
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
    @unused inlineContext: InlineContext
  ): IR.Expression = ir

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr

  /** A container for annotations on an IR construct.
    *
    * @param annotations the initial annotations for the container
    */
  case class Annotations(annotations: Seq[IR.Name.Annotation])
      extends IRPass.Metadata {
    override val metadataName: String                 = "Annotations"
    override def duplicate(): Option[IRPass.Metadata] = Some(this.copy())

    /** Add an annotation to the annotations container.
      *
      * @param annotation the annotation to add
      * @return `this`, with `annotation` added to it
      */
    def addAnnotation(annotation: IR.Name.Annotation): Annotations =
      this.copy(annotations = this.annotations :+ annotation)

    /** @inheritdoc */
    override def prepareForSerialization(compiler: Compiler): Annotations = {
      annotations.foreach(ir =>
        ir.preorder.foreach(_.passData.prepareForSerialization(compiler))
      )
      this
    }

    /** @inheritdoc */
    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[IRPass.Metadata] = {
      annotations.foreach { ann =>
        ann.preorder.foreach { ir =>
          if (!ir.passData.restoreFromSerialization(compiler)) {
            return None
          }
        }
      }

      Some(this)
    }
  }
}
