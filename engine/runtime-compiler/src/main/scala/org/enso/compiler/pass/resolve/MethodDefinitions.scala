package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.{
  DefinitionArgument,
  Expression,
  Function,
  Module,
  Name
}
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
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
              method.copy(
                methodReference = resolvedMethodRef,
                isStatic =
                  definition.Method.Explicit.computeIsStatic(method.body)
              )
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
            org.enso.common.Asserts
              .assertInJvm(method.body.isInstanceOf[Function.Lambda])
            val dup = method.duplicate()
            // This is the self argument that will receive the `SelfType.type` value upon dispatch, it is added to avoid modifying the dispatch mechanism.
            val syntheticModuleSelfArg = DefinitionArgument.Specified(
              Name.Self(identifiedLocation = null, synthetic = true),
              None,
              None,
              suspended          = false,
              identifiedLocation = null
            )

            // The actual `self` argument that is referenced inside of method body is the second one in the lambda.
            // This is the argument that will hold the actual instance of the object we are calling on, e.g. `My_Type.method instance`.
            // We add a type check to it to ensure only `instance` of `My_Type` can be passed to it.
            val static = dup.copy(
              body = new Function.Lambda(
                // This is the synthetic Self argument that gets the static module
                List(syntheticModuleSelfArg),
                // Here we add the type ascription ensuring that the 'proper' self argument only accepts _instances_ of the type (or triggers conversions)
                addTypeAscriptionToSelfArgument(dup.body),
                identifiedLocation = null
              ),
              isStaticWrapperForInstanceMethod = true,
              isStatic                         = true
            )
            List(method, static)
          case _ =>
            List(method)
        }

      case other => List(other)
    }

    ir.copy(bindings = withStaticAliases)
  }

  private def addTypeAscriptionToSelfArgument(
    methodBody: Expression
  ): Expression = methodBody match {
    case lambda: Function.Lambda =>
      val newArguments = lambda.arguments match {
        case (selfArg @ DefinitionArgument.Specified(
              Name.Self(_, _, _),
              _,
              _,
              _,
              _,
              _
            )) :: rest =>
          val selfType =
            Name.SelfType(identifiedLocation = selfArg.identifiedLocation)
          selfArg.copy(ascribedType = Some(selfType)) :: rest
        case other :: _ =>
          throw new CompilerError(
            s"MethodDefinitions pass: expected the first argument to be `self`, but got $other"
          )
        case Nil =>
          throw new CompilerError(
            s"MethodDefinitions pass: expected at least one argument (self) in the method, but got none."
          )
      }
      lambda.copy(arguments = newArguments)
    case other =>
      throw new CompilerError(
        s"Unexpected body type $other in MethodDefinitions pass."
      )
  }

  // Generate static wrappers for
  // 1. Types having at least one type constructor
  // 2. All builtin types except for Nothing. Nothing's eigentype is Nothing and not Nothing.type,
  //    would lead to overriding conflicts.
  //    TODO: Remove the hardcoded type once Enso's annotations can define parameters.
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
          case Right(resolvedItems) =>
            org.enso.common.Asserts.assertInJvm(
              resolvedItems.size == 1,
              "Expected a single resolution"
            )
            resolvedItems.head match {
              case _: BindingsMap.ResolvedConstructor =>
                errors.Resolution(
                  typePointer,
                  errors.Resolution.UnexpectedConstructor(
                    "a method definition target"
                  )
                )
              case value: BindingsMap.ResolvedModule =>
                typePointer.updateMetadata(
                  new MetadataPair(this, BindingsMap.Resolution(value))
                )
              case value: BindingsMap.ResolvedType =>
                typePointer.updateMetadata(
                  new MetadataPair(this, BindingsMap.Resolution(value))
                )
              case _: BindingsMap.ResolvedPolyglotSymbol =>
                errors.Resolution(
                  typePointer,
                  errors.Resolution.UnexpectedPolyglot(
                    "a method definition target"
                  )
                )
              case _: BindingsMap.ResolvedPolyglotField =>
                errors.Resolution(
                  typePointer,
                  errors.Resolution.UnexpectedPolyglot(
                    "a method definition target"
                  )
                )
              case _: BindingsMap.ResolvedModuleMethod =>
                errors.Resolution(
                  typePointer,
                  errors.Resolution.UnexpectedMethod(
                    "a method definition target"
                  )
                )
              case _: BindingsMap.ResolvedExtensionMethod =>
                errors.Resolution(
                  typePointer,
                  errors.Resolution.UnexpectedMethod(
                    "a static method definition target"
                  )
                )
              case _: BindingsMap.ResolvedConversionMethod =>
                errors.Resolution(
                  typePointer,
                  errors.Resolution.UnexpectedMethod(
                    "a conversion method definition target"
                  )
                )
            }
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
