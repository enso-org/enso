package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.interpreter.runtime.Module

object BindingResolution extends IRPass {

  override type Metadata = LocalBindings

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
    val definedConstructors = ir.bindings.collect {
      case cons: IR.Module.Scope.Definition.Atom => Cons(cons.name)
    }
    val definedMethods = ir.bindings.collect {
      case method: IR.Module.Scope.Definition.Method =>
        Method(method.methodReference)
    }
    ir.updateMetadata(
      this -->> LocalBindings(definedConstructors, definedMethods)
    )
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
  ): IR.Expression = ir

  case class LocalBindings(types: List[Cons], methods: List[Method])
      extends IRPass.Metadata {

    /** The name of the metadata as a string. */
    override val metadataName: String = "Local Bindings"

    /** Creates a duplicate of this metadata if applicable.
      *
      * This method should employ deep-copy semantics where appropriate. It may
      * return None to indicate that this metadata should not be preserved
      * during duplication.
      *
      * @return Some duplicate of this metadata or None if this metadata should
      *         not be preserved
      */
    override def duplicate(): Option[IRPass.Metadata] = Some(this.copy(types, methods))

    var resolvedImports: List[Module] = List()
  }

  case class Cons(name: IR.Name)
  case class Method(ref: IR.Name.MethodReference)
}
