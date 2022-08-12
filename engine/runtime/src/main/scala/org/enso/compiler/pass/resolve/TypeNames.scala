package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.Resolution
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.BindingAnalysis

import scala.annotation.unused

/** Resolves and desugars referent name occurences in type positions.
  */
case object TypeNames extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = BindingsMap.Resolution

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] =
    Seq(BindingAnalysis)

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
    val bindingsMap =
      ir.unsafeGetMetadata(BindingAnalysis, "bindings analysis did not run")
    ir.copy(bindings = ir.bindings.map { d =>
      val mapped = d.mapExpressions(resolveExpression(bindingsMap, _))
      doResolveType(bindingsMap, mapped)
    })
  }

  private def resolveExpression(
    bindingsMap: BindingsMap,
    ir: IR.Expression
  ): IR.Expression = {
    def go(ir: IR.Expression): IR.Expression = {
      doResolveType(bindingsMap, ir.mapExpressions(go))
    }
    go(ir)
  }

  private def doResolveType[T <: IR](bindingsMap: BindingsMap, ir: T): T = {
    ir.getMetadata(TypeSignatures)
      .map { s =>
        ir.updateMetadata(
          TypeSignatures -->> TypeSignatures.Signature(
            resolveSignature(bindingsMap, s.signature)
          )
        )
      }
      .getOrElse(ir)
  }

  private def resolveSignature(
    bindingsMap: BindingsMap,
    expression: IR.Expression
  ): IR.Expression =
    expression.transformExpressions { case n: IR.Name.Literal =>
      bindingsMap
        .resolveName(n.name)
        .map(res => n.updateMetadata(this -->> Resolution(res)))
        .getOrElse(n)
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
    ir
  }

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr
}
