package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{Resolution, ResolvedType}
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
          methodRef.typePointer.map(resolveType(_, availableSymbolsMap))
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

    val withStaticAliases = newDefs.flatMap {
      case method: IR.Module.Scope.Definition.Method.Explicit
          if !method.isStatic =>
        method.methodReference.typePointer.flatMap(
          _.getMetadata(this)
        ) match {
          case Some(Resolution(ResolvedType(_, tp))) if tp.members.nonEmpty =>
            val dup = method.duplicate()
            val static = dup.copy(body =
              IR.Function.Lambda(
                List(
                  IR.DefinitionArgument
                    .Specified(
                      IR.Name.Self(None, true),
                      None,
                      None,
                      false,
                      None
                    )
                ),
                dup.body,
                None
              )
            )
            List(method, static)
          case _ => List(method)
        }

      case other => List(other)
    }

    ir.copy(bindings = withStaticAliases)
  }

  private def resolveType(
    typePointer: IR.Name,
    availableSymbolsMap: BindingsMap
  ): IR.Name = {
    typePointer match {
      case _: IR.Name.Qualified | _: IR.Name.Literal =>
        val items = typePointer match {
          case IR.Name.Qualified(names, _, _, _) => names.map(_.name)
          case IR.Name.Literal(name, _, _, _, _) => List(name)
          case _ =>
            throw new CompilerError("Impossible to reach.")
        }
        availableSymbolsMap.resolveQualifiedName(items) match {
          case Left(err) =>
            IR.Error.Resolution(
              typePointer,
              IR.Error.Resolution.ResolverError(err)
            )
          case Right(_: BindingsMap.ResolvedConstructor) =>
            IR.Error.Resolution(
              typePointer,
              IR.Error.Resolution.UnexpectedConstructor(
                "a method definition target"
              )
            )
          case Right(value: BindingsMap.ResolvedModule) =>
            typePointer.updateMetadata(
              this -->> BindingsMap.Resolution(value)
            )
          case Right(value: BindingsMap.ResolvedType) =>
            typePointer.updateMetadata(this -->> BindingsMap.Resolution(value))
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
