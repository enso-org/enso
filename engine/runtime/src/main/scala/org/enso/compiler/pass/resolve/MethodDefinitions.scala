package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.compiler.pass.desugar.{
  ComplexType,
  FunctionBinding,
  GenerateMethodBodies
}

/**
  * Resolves the correct `this` argument type for methods definitions
  * and stores the resolution in the method's metadata.
  */
case object MethodDefinitions extends IRPass {

  override type Metadata = BindingsMap.Resolution

  override type Config = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] =
    List(ComplexType, FunctionBinding, GenerateMethodBodies, BindingAnalysis)

  override val invalidatedPasses: Seq[IRPass] = List()

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
    val availableSymbolsMap = ir.unsafeGetMetadata(
      BindingAnalysis,
      "MethodDefinitionResolution is being run before BindingResolution"
    )
    val newDefs = ir.bindings.map {
      case method: IR.Module.Scope.Definition.Method.Explicit =>
        val methodRef = method.methodReference
        val resolvedTypeRef = methodRef.typePointer match {
          case tp: IR.Name.Here =>
            tp.updateMetadata(
              this -->> BindingsMap.Resolution(
                BindingsMap.ResolvedModule(availableSymbolsMap.currentModule)
              )
            )
          case tp @ IR.Name.Qualified(List(item), _, _, _) =>
            availableSymbolsMap.resolveUppercaseName(item.name) match {
              case Left(err) =>
                IR.Error.Resolution(tp, IR.Error.Resolution.Reason(err))
              case Right(candidate) =>
                tp.updateMetadata(this -->> BindingsMap.Resolution(candidate))
            }
          case tp: IR.Error.Resolution => tp
          case _ =>
            throw new CompilerError(
              "Unexpected kind of name for method reference"
            )
        }
        val resolvedMethodRef = methodRef.copy(typePointer = resolvedTypeRef)
        val resolvedMethod    = method.copy(methodReference = resolvedMethodRef)
        resolvedMethod
      case other => other
    }

    ir.copy(bindings = newDefs)
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
