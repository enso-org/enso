package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.{Expression, Module, Name, Pattern}
import org.enso.compiler.core.ir.expression.{errors, Case}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{AliasAnalysis, BindingAnalysis}
import org.enso.compiler.pass.desugar.{GenerateMethodBodies, NestedPatternMatch}

/** Resolves constructors in pattern matches and validates their arity.
  */
object Patterns extends IRPass {

  override type Metadata = BindingsMap.Resolution
  override type Config   = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRPass] =
    Seq(NestedPatternMatch, GenerateMethodBodies, BindingAnalysis)
  override lazy val invalidatedPasses: Seq[IRPass] = Seq(AliasAnalysis)

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
    val bindings = ir.unsafeGetMetadata(
      BindingAnalysis,
      "Binding resolution was not run before pattern resolution"
    )
    ir.copy(bindings = ir.bindings.map(doDefinition(_, bindings)))
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
  ): Expression = {
    val bindings = inlineContext.bindingsAnalysis()
    doExpression(ir, bindings, None)
  }

  private def doDefinition(
    ir: Definition,
    bindings: BindingsMap
  ): Definition = {
    ir match {
      case method: definition.Method.Explicit =>
        val resolution = method.methodReference.typePointer
          .flatMap(
            _.getMetadata(MethodDefinitions)
          )
          .map(_.target)
        val newBody = doExpression(method.body, bindings, resolution)
        method.copy(body = newBody)
      case _ => ir.mapExpressions(doExpression(_, bindings, None))
    }
  }

  /** Just delegates to the same-named method from [[BindingsMap]]
    * and expects a single resolution.
    */
  private def resolveSingleQualifiedName(
    bindingsMap: BindingsMap,
    parts: List[String]
  ): Either[BindingsMap.ResolutionError, BindingsMap.ResolvedName] = {
    bindingsMap.resolveQualifiedName(parts) match {
      case Left(err) => Left(err)
      case Right(resolvedNames) =>
        org.enso.common.Asserts
          .assertInJvm(resolvedNames.size == 1, "Expected a single resolution")
        Right(resolvedNames.head)
    }
  }

  /** @inheritdoc [[resolveSingleQualifiedName]]
    */
  private def resolveSingleQualifiedNameIn(
    bindingsMap: BindingsMap,
    scope: BindingsMap.ResolvedName,
    submoduleNames: List[String],
    finalItem: String
  ): Either[BindingsMap.ResolutionError, BindingsMap.ResolvedName] = {
    bindingsMap.resolveQualifiedNameIn(scope, submoduleNames, finalItem) match {
      case Left(err) => Left(err)
      case Right(resolvedNames) =>
        org.enso.common.Asserts
          .assertInJvm(resolvedNames.size == 1, "Expected a single resolution")
        Right(resolvedNames.head)
    }
  }

  /** @inheritdoc [[resolveSingleQualifiedName]]
    */
  private def resolveSingleName(
    bindingsMap: BindingsMap,
    name: String
  ): Either[BindingsMap.ResolutionError, BindingsMap.ResolvedName] = {
    bindingsMap.resolveName(name) match {
      case Left(err) => Left(err)
      case Right(resolvedNames) =>
        org.enso.common.Asserts
          .assertInJvm(resolvedNames.size == 1, "Expected a single resolution")
        Right(resolvedNames.head)
    }
  }

  private def doExpression(
    expr: Expression,
    bindings: BindingsMap,
    selfTypeResolution: Option[BindingsMap.ResolvedName]
  ): Expression = {
    expr.transformExpressions { case caseExpr: Case.Expr =>
      val newBranches = caseExpr.branches.map { branch =>
        val resolvedPattern = branch.pattern match {
          case consPat: Pattern.Constructor =>
            val consName = consPat.constructor
            val resolution = consName match {
              case qual: Name.Qualified =>
                qual.parts match {
                  case (_: Name.SelfType) :: (others :+ item) =>
                    selfTypeResolution.map(
                      resolveSingleQualifiedNameIn(
                        bindings,
                        _,
                        others.map(_.name),
                        item.name
                      )
                    )
                  case _ =>
                    val parts = qual.parts.map(_.name)
                    Some(
                      resolveSingleQualifiedName(bindings, parts)
                    )
                }
              case lit: Name.Literal =>
                Some(resolveSingleName(bindings, lit.name))
              case _: Name.SelfType =>
                selfTypeResolution.map(Right(_))
              case _ => None
            }
            val resolvedName: Name = resolution
              .map {
                case Left(err) =>
                  val r = errors.Resolution(
                    consPat.constructor,
                    errors.Resolution.ResolverError(err)
                  )
                  r.setLocation(consPat.location)
                case Right(value: BindingsMap.ResolvedConstructor) =>
                  consName.updateMetadata(
                    new MetadataPair(this, BindingsMap.Resolution(value))
                  )
                case Right(value: BindingsMap.ResolvedModule) =>
                  consName.updateMetadata(
                    new MetadataPair(this, BindingsMap.Resolution(value))
                  )
                case Right(value: BindingsMap.ResolvedType) =>
                  consName.updateMetadata(
                    new MetadataPair(this, BindingsMap.Resolution(value))
                  )
                case Right(value: BindingsMap.ResolvedPolyglotSymbol) =>
                  consName.updateMetadata(
                    new MetadataPair(this, BindingsMap.Resolution(value))
                  )
                case Right(value: BindingsMap.ResolvedPolyglotField) =>
                  consName.updateMetadata(
                    new MetadataPair(this, BindingsMap.Resolution(value))
                  )

                case Right(_: BindingsMap.ResolvedModuleMethod) =>
                  val r = errors.Resolution(
                    consName,
                    errors.Resolution.UnexpectedMethod(
                      "method inside pattern match"
                    )
                  )
                  r.setLocation(consName.location)
                case Right(_: BindingsMap.ResolvedExtensionMethod) =>
                  val r = errors.Resolution(
                    consName,
                    errors.Resolution.UnexpectedMethod(
                      "static method inside pattern match"
                    )
                  )
                  r.setLocation(consName.location)
                case Right(_: BindingsMap.ResolvedConversionMethod) =>
                  val r = errors.Resolution(
                    consName,
                    errors.Resolution.UnexpectedMethod(
                      "conversion method inside pattern match"
                    )
                  )
                  r.setLocation(consName.location)
                case Right(_) =>
                  throw new CompilerError(
                    "Impossible, should be transformed into an error before."
                  )
              }
              .getOrElse(consName)

            val actualResolution = resolvedName.getMetadata(this)
            val expectedArity = actualResolution.map { res =>
              res.target match {
                case BindingsMap.ResolvedConstructor(_, cons) => cons.arity
                case BindingsMap.ResolvedModule(_)            => 0
                case BindingsMap.ResolvedPolyglotSymbol(_, _) => 0
                case BindingsMap.ResolvedPolyglotField(_, _)  => 0
                case BindingsMap.ResolvedModuleMethod(_, _) =>
                  throw new CompilerError(
                    "Impossible, should be transformed into an error before."
                  )
                case BindingsMap.ResolvedExtensionMethod(_, _) =>
                  throw new CompilerError(
                    "Impossible, should be transformed into an error before."
                  )
                case BindingsMap.ResolvedConversionMethod(_, _) =>
                  throw new CompilerError(
                    "Impossible, should be transformed into an error before."
                  )
                case BindingsMap.ResolvedType(_, _) => 0
              }
            }
            expectedArity match {
              case Some(arity) =>
                if (consPat.fields.length != arity) {
                  errors.Pattern(
                    consPat,
                    errors.Pattern.WrongArity(
                      consPat.constructor.name,
                      arity,
                      consPat.fields.length
                    )
                  )
                } else {
                  consPat.copy(constructor = resolvedName)
                }
              case None => consPat.copy(constructor = resolvedName)
            }
          case tpePattern @ Pattern.Type(_, tpeName, _, _) =>
            val resolution = tpeName match {
              case qual: Name.Qualified =>
                val parts = qual.parts.map(_.name)
                Some(
                  resolveSingleQualifiedName(bindings, parts)
                )
              case lit: Name.Literal =>
                Some(resolveSingleName(bindings, lit.name))
              case _: Name.SelfType =>
                selfTypeResolution.map(Right(_))
              case _ => None
            }
            val resolvedTpeName = resolution
              .map {
                case Left(err) =>
                  errors.Resolution(
                    tpePattern.tpe,
                    errors.Resolution.ResolverError(err)
                  )
                case Right(value: BindingsMap.ResolvedType) =>
                  tpeName.updateMetadata(
                    new MetadataPair(this, BindingsMap.Resolution(value))
                  )
                case Right(_: BindingsMap.ResolvedConstructor) =>
                  errors.Resolution(
                    tpeName,
                    errors.Resolution
                      .UnexpectedConstructor(s"type pattern case")
                  )
                case Right(value: BindingsMap.ResolvedPolyglotSymbol) =>
                  tpeName.updateMetadata(
                    new MetadataPair(this, BindingsMap.Resolution(value))
                  )
                case Right(value: BindingsMap.ResolvedPolyglotField) =>
                  tpeName.updateMetadata(
                    new MetadataPair(this, BindingsMap.Resolution(value))
                  )
                /*errors.Resolution(
                    tpeName,
                    errors.Resolution.UnexpectedPolyglot(s"type pattern case")
                  )*/
                case Right(_: BindingsMap.ResolvedModuleMethod) =>
                  errors.Resolution(
                    tpeName,
                    errors.Resolution
                      .UnexpectedMethod(s"method type pattern case")
                  )
                case Right(_: BindingsMap.ResolvedExtensionMethod) =>
                  errors.Resolution(
                    tpeName,
                    errors.Resolution.UnexpectedMethod(
                      s"static method inside type pattern case"
                    )
                  )
                case Right(_: BindingsMap.ResolvedConversionMethod) =>
                  errors.Resolution(
                    tpeName,
                    errors.Resolution.UnexpectedMethod(
                      s"conversion method inside type pattern case"
                    )
                  )
                case Right(_: BindingsMap.ResolvedModule) =>
                  errors.Resolution(
                    tpeName,
                    errors.Resolution
                      .UnexpectedModule(s"module inside type pattern case")
                  )
              }
              .getOrElse(tpeName)

            tpePattern.copy(tpe = resolvedTpeName)
          case other => other
        }
        branch.copy(
          pattern = resolvedPattern,
          expression =
            doExpression(branch.expression, bindings, selfTypeResolution)
        )
      }
      caseExpr.copy(branches = newBranches)

    }
  }
}
