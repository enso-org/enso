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

import scala.annotation.unused

/** Resolves the correct `this` argument type for method definitions and stores
  * the resolution in the method's metadata.
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
      case method: IR.Module.Scope.Definition.Method =>
        val methodRef = method.methodReference
        val resolvedTypeRef =
          resolveType(methodRef.typePointer, availableSymbolsMap)
        val resolvedMethodRef = methodRef.copy(typePointer = resolvedTypeRef)

        method match {
          case method: IR.Module.Scope.Definition.Method.Explicit =>
            val resolvedMethod =
              method.copy(methodReference = resolvedMethodRef)
            resolvedMethod
          case method: IR.Module.Scope.Definition.Method.Conversion =>
            val sourceTypeExpr = method.sourceTypeName

            val resolvedName: IR.Name = sourceTypeExpr match {
              case name: IR.Name => resolveType(name, availableSymbolsMap)
              case _ =>
                IR.Error.Conversion(
                  sourceTypeExpr,
                  IR.Error.Conversion.UnsupportedSourceType
                )
            }

            val resolvedMethod = method.copy(
              methodReference = resolvedMethodRef,
              sourceTypeName  = resolvedName
            )
            resolvedMethod

          case _ =>
            throw new CompilerError(
              "Unexpected method type in MethodDefinitions pass."
            )
        }
      case other => other
    }

    ir.copy(bindings = newDefs)
  }

  private def resolveType(
    typePointer: IR.Name,
    availableSymbolsMap: BindingsMap
  ): IR.Name = {
    typePointer match {
      case tp: IR.Name.Here =>
        tp.updateMetadata(
          this -->> BindingsMap.Resolution(
            BindingsMap.ResolvedModule(availableSymbolsMap.currentModule)
          )
        )
      case _: IR.Name.Qualified | _: IR.Name.Literal =>
        val items = typePointer match {
          case IR.Name.Qualified(names, _, _, _)    => names.map(_.name)
          case IR.Name.Literal(name, _, _, _, _, _) => List(name)
          case _ =>
            throw new CompilerError("Impossible to reach.")
        }
        availableSymbolsMap.resolveQualifiedName(items) match {
          case Left(err) =>
            IR.Error.Resolution(
              typePointer,
              IR.Error.Resolution.ResolverError(err)
            )
          case Right(value: BindingsMap.ResolvedConstructor) =>
            typePointer.updateMetadata(
              this -->> BindingsMap.Resolution(value)
            )
          case Right(value: BindingsMap.ResolvedModule) =>
            typePointer.updateMetadata(
              this -->> BindingsMap.Resolution(value)
            )
          case Right(_: BindingsMap.ResolvedPolyglotSymbol) =>
            IR.Error.Resolution(
              typePointer,
              IR.Error.Resolution.UnexpectedPolyglot(
                "a method definition target"
              )
            )
          case Right(_: BindingsMap.ResolvedMethod) =>
            IR.Error.Resolution(
              typePointer,
              IR.Error.Resolution.UnexpectedMethod(
                "a method definition target"
              )
            )
        }
      case tp: IR.Error.Resolution => tp
      case _ =>
        throw new CompilerError(
          "Unexpected kind of name for method reference"
        )
    }
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

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr
}
