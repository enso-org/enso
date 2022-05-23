package org.enso.compiler.pass.analyse

import org.enso.compiler.Compiler
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.pass.IRPass

import scala.annotation.unused

/** A pass that traverses the given root IR and accumulates all the encountered
  * diagnostic nodes in the root.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object GatherDiagnostics extends IRPass {
  override type Metadata = DiagnosticsMeta
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass]   = List()
  override val invalidatedPasses: Seq[IRPass] = List()

  /** Executes the pass on the provided `ir`, and attaches all the encountered
    * diagnostics to its metadata storage.
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
  ): IR.Module =
    ir.updateMetadata(this -->> gatherMetadata(ir))

  /** Executes the pass on the provided `ir`, and attaches all the encountered
    * diagnostics to its metadata storage.
    *
    * @param ir the IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir` with all the errors accumulated in pass metadata.
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = ir.updateMetadata(this -->> gatherMetadata(ir))

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr

  /** Gathers diagnostics from all children of an IR node.
    *
    * @param ir the node to gather diagnostics from
    * @return `ir`, with all diagnostics from its subtree associated with it
    */
  private def gatherMetadata(ir: IR): DiagnosticsMeta = {
    val diagnostics = ir.preorder.collect {
      case err: IR.Diagnostic => List(err)
      case x                  => x.diagnostics.toList
    }.flatten
    DiagnosticsMeta(
      diagnostics.distinctBy(d => new DiagnosticKeys(d))
    )
  }

  final private class DiagnosticKeys(private val diagnostic: IR.Diagnostic) {

    /** Equals is based on type of diagnostic, its location and its diagnostic keys.
      */
    override def equals(any: Any): Boolean = any match {
      case other: DiagnosticKeys =>
        diagnostic.getClass == other.diagnostic.getClass &&
        diagnostic.location == other.diagnostic.location &&
        java.util.Arrays.equals(diagnostic.diagnosticKeys(), other.diagnostic.diagnosticKeys())
      case _ => false
    }

    /** Hascode computed from location and provided diagnostic keys */
    override def hashCode(): Int = {
      var sum = diagnostic.location.hashCode
      for (k <- diagnostic.diagnosticKeys()) {
        sum += k.hashCode
      }
      return sum
    }
  }

  /** A container for diagnostics found in the IR.
    *
    * @param diagnostics a list of the errors found in the IR
    */
  case class DiagnosticsMeta(diagnostics: List[IR.Diagnostic])
      extends IRPass.Metadata {

    /** The name of the metadata as a string. */
    override val metadataName: String = "GatherDiagnostics.Diagnostics"

    override def duplicate(): Option[IRPass.Metadata] =
      Some(this)

    /** @inheritdoc */
    override def prepareForSerialization(compiler: Compiler): DiagnosticsMeta =
      this

    /** @inheritdoc */
    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[DiagnosticsMeta] = Some(this)
  }
}
