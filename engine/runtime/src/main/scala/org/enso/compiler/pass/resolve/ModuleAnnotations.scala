package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{Expression, Module}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.Name
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.core.ir.expression.Comment
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.{
  ComplexType,
  FunctionBinding,
  GenerateMethodBodies
}

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
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    var lastAnnotations: Seq[Name.Annotation] = Seq()
    val newBindings = ir.bindings.map {
      case ann: Name.BuiltinAnnotation =>
        lastAnnotations :+= ann
        None
      case ann: Name.GenericAnnotation =>
        Some(ann)
      case comment: Comment => Some(comment)
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
    var lastAnnotations: Seq[Name.Annotation] = Seq()
    val newBodyElems = typ.body.flatMap {
      case ann: Name.BuiltinAnnotation =>
        lastAnnotations :+= ann
        None
      case ann: Name.GenericAnnotation =>
        Some(ann)
      case comment: Comment => Some(comment)
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
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = ir

  /** @inheritdoc */

  /** A container for annotations on an IR construct.
    *
    * @param annotations the initial annotations for the container
    */
  case class Annotations(annotations: Seq[Name.Annotation])
      extends IRPass.IRMetadata {
    override val metadataName: String                   = "Annotations"
    override def duplicate(): Option[IRPass.IRMetadata] = Some(this.copy())

    /** Add an annotation to the annotations container.
      *
      * @param annotation the annotation to add
      * @return `this`, with `annotation` added to it
      */
    def addAnnotation(annotation: Name.Annotation): Annotations =
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
    ): Option[IRPass.IRMetadata] = {
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
