package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{
  DefinitionArgument,
  Expression,
  Function,
  Module,
  Name
}
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{Resolution, ResolvedType, Type}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.compiler.pass.desugar.{
  ComplexType,
  FunctionBinding,
  GenerateMethodBodies
}

/** Resolves the correct `self` argument type for method definitions and stores
  * the resolution in the method's metadata.
  */
case object MethodDefinitions extends IRPass {

  override type Metadata = BindingsMap.Resolution

  override type Config = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRPass] =
    List(ComplexType, FunctionBinding, GenerateMethodBodies, BindingAnalysis)

  override lazy val invalidatedPasses: Seq[IRPass] = List()

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
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    val availableSymbolsMap = ir.unsafeGetMetadata(
      BindingAnalysis,
      "MethodDefinitionResolution is being run before BindingResolution"
    )
    val newDefs = ir.bindings.map {
      case method: definition.Method =>
        val methodRef = method.methodReference
        val resolvedTypeRef =
          methodRef.typePointer.map(resolveType(_, availableSymbolsMap))
        val resolvedMethodRef = methodRef.copy(typePointer = resolvedTypeRef)

        method match {
          case method: definition.Method.Explicit =>
            val resolvedMethod =
              method.copy(methodReference = resolvedMethodRef)
            resolvedMethod
          case method: definition.Method.Conversion =>
            val sourceTypeExpr = method.sourceTypeName

            val resolvedName: Name = sourceTypeExpr match {
              case name: Name => resolveType(name, availableSymbolsMap)
              case _ =>
                errors.Conversion(
                  sourceTypeExpr,
                  errors.Conversion.UnsupportedSourceType
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
      case method: definition.Method.Explicit if !method.isStatic =>
        method.methodReference.typePointer.flatMap(
          _.getMetadata(this)
        ) match {
          case Some(Resolution(ResolvedType(_, tp)))
              if canGenerateStaticWrappers(tp) =>
            val dup = method.duplicate()
            val static = dup.copy(body =
              Function.Lambda(
                List(
                  DefinitionArgument
                    .Specified(
                      Name.Self(None, true),
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
          case _ =>
            List(method)
        }

      case other => List(other)
    }

    ir.copy(bindings = withStaticAliases)
  }

  // Generate static wrappers for
  // 1. Types having at least one type constructor
  // 2. All builtin types except for Nothing. Nothing's eigentype is Nothing and not Nothing.type,
  //    would lead to overriding conflicts.
  //    TODO: Remvoe the hardcoded type once Enso's annotations can define parameters.
  private def canGenerateStaticWrappers(tp: Type): Boolean =
    tp.members.nonEmpty || (tp.builtinType && (tp.name != "Nothing"))

  private def resolveType(
    typePointer: Name,
    availableSymbolsMap: BindingsMap
  ): Name = {
    typePointer match {
      case _: Name.Qualified | _: Name.Literal =>
        val items = typePointer match {
          case qualName: Name.Qualified => qualName.parts.map(_.name)
          case literal: Name.Literal    => List(literal.name)
          case _ =>
            throw new CompilerError("Impossible to reach.")
        }
        availableSymbolsMap.resolveQualifiedName(items) match {
          case Left(err) =>
            errors.Resolution(
              typePointer,
              errors.Resolution.ResolverError(err)
            )
          case Right(_: BindingsMap.ResolvedConstructor) =>
            errors.Resolution(
              typePointer,
              errors.Resolution.UnexpectedConstructor(
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
            errors.Resolution(
              typePointer,
              errors.Resolution.UnexpectedPolyglot(
                "a method definition target"
              )
            )
          case Right(_: BindingsMap.ResolvedPolyglotField) =>
            errors.Resolution(
              typePointer,
              errors.Resolution.UnexpectedPolyglot(
                "a method definition target"
              )
            )
          case Right(_: BindingsMap.ResolvedMethod) =>
            errors.Resolution(
              typePointer,
              errors.Resolution.UnexpectedMethod(
                "a method definition target"
              )
            )

        }
      case tp: errors.Resolution => tp
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
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = ir

}
