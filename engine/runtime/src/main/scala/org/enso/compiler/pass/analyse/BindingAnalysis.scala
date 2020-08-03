package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.{
  ComplexType,
  FunctionBinding,
  GenerateMethodBodies
}
import org.enso.compiler.pass.resolve.{MethodDefinitions, Patterns}

/**
  * Recognizes all defined bindings in the current module and constructs
  * a mapping data structure that can later be used for symbol resolution.
  */
case object BindingAnalysis extends IRPass {

  override type Metadata = BindingsMap

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] =
    Seq(ComplexType, FunctionBinding, GenerateMethodBodies)

  /** The passes that are invalidated by running this pass. */
  override val invalidatedPasses: Seq[IRPass] =
    Seq(MethodDefinitions, Patterns)

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
      case cons: IR.Module.Scope.Definition.Atom =>
        BindingsMap.Cons(cons.name.name, cons.arguments.length)
    }
    ir.updateMetadata(
      this -->> BindingsMap(
        definedConstructors,
        moduleContext.module
      )
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

}
