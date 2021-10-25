package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.pass.IRPass
import org.enso.docs.generator.DocParserWrapper

import scala.annotation.unused

/** Generates documentation on resolved IR.
  */
case object GenerateDocumentation extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = DocumentationComments.Doc

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] = Seq(DocumentationComments)

  /** The passes that are invalidated by running this pass. */
  override val invalidatedPasses: Seq[IRPass] = Seq()

  /** Collects comments for a module and assigns them to the commented
    * entities as metadata.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    if (moduleContext.isGeneratingDocs) {
      resolveModule(ir)
    } else {
      ir
    }
  }

  /** Acts as an identity function to conform with the interface.
    *
    * @param ir the Enso IR to process.
    * @param inlineContext a context object that contains the information
    *                      needed for inline evaluation.
    * @return unchanged ir.
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = ir

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr

  // === Pass Internals =======================================================

  /** Resolves documentation comments in a module.
    *
    * @param ir the module to resolve comments in
    * @return `ir`, with any doc comments associated with nodes as metadata
    */
  private def resolveModule(ir: IR.Module): IR.Module = {
    val newBindings = ir.bindings.map { x =>
      val doc = x.getMetadata(DocumentationComments)
      doc match {
        case Some(value) =>
          val genDoc = DocParserWrapper.runOnPureDoc(value.documentation)
          x.updateMetadata(this -->> DocumentationComments.Doc(genDoc))
        case None => x
      }
    }
    ir.copy(bindings = newBindings)
  }
}
