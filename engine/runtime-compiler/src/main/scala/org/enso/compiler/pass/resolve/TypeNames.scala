package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{Expression, Function, Module, Name}
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition.Method
import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.core.ir.`type`
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{Resolution, ResolvedModule}
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.BindingAnalysis

/** Resolves and desugars referent name occurrences in type positions.
  */
case object TypeNames extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = BindingsMap.Resolution

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override lazy val precursorPasses: Seq[IRPass] =
    Seq(BindingAnalysis)

  /** The passes that are invalidated by running this pass. */
  override lazy val invalidatedPasses: Seq[IRPass] = Seq()

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
    val bindingsMap =
      ir.unsafeGetMetadata(BindingAnalysis, "bindings analysis did not run")
    ir.copy(bindings = ir.bindings.map { d =>
      // TODO the 2 pattern matches next to each other could be cleaned up
      val typeParams: List[Name] = d match {
        case t: Definition.Type => t.params.map(_.name)
        case m: Method.Explicit =>
          val params: List[Name] = m.methodReference.typePointer
            .flatMap { p =>
              p.getMetadata(MethodDefinitions)
                .map(_.target match {
                  case typ: BindingsMap.ResolvedType =>
                    typ.tp.params.map(Name.Literal(_, false, None)).toList
                  case _ => List()
                })
            }
            .getOrElse(List())
          params
        case _ => Nil
      }
      val selfType: Option[BindingsMap.ResolvedType] = d match {
        case m: Method.Explicit =>
          m.methodReference.typePointer
            .flatMap { p =>
              p.getMetadata(MethodDefinitions)
                .flatMap(_.target match {
                  case typ: BindingsMap.ResolvedType => Some(typ)
                  case _ => None
                })
            }
        case _ => None
      }
      val mapped =
        d.mapExpressions(resolveExpression(selfType, typeParams, bindingsMap, _))
      doResolveType(
        selfType,
        typeParams,
        bindingsMap,
        mapped match {
          case typ: Definition.Type =>
            typ.members.foreach(m =>
              m.arguments.foreach(a =>
                doResolveType(selfType, typ.params.map(_.name), bindingsMap, a)
              )
            )
            typ
          case x => x
        }
      )
    })
  }

  private def resolveExpression(
    selfType: Option[BindingsMap.ResolvedType],
    typeParams: List[Name],
    bindingsMap: BindingsMap,
    ir: Expression
  ): Expression = {
    def go(ir: Expression): Expression = {
      val processedIr = ir match {
        case fn: Function.Lambda =>
          fn.copy(arguments =
            fn.arguments.map(doResolveType(selfType, typeParams, bindingsMap, _))
          )
        case x => x
      }
      doResolveType(selfType, typeParams, bindingsMap, processedIr.mapExpressions(go))
    }
    go(ir)
  }

  private def doResolveType[T <: IR](
    selfType: Option[BindingsMap.ResolvedType],
    typeParams: List[Name],
    bindingsMap: BindingsMap,
    ir: T
  ): T = {
    ir.getMetadata(TypeSignatures)
      .map { s =>
        ir.updateMetadata(
          new MetadataPair(
            TypeSignatures,
            TypeSignatures.Signature(
              resolveSignature(selfType, typeParams, bindingsMap, s.signature),
              s.comment
            )
          )
        )
      }
      .getOrElse(ir)
  }

  private def resolveSignature(
    selfType: Option[BindingsMap.ResolvedType],
    typeParams: List[Name],
    bindingsMap: BindingsMap,
    expression: Expression
  ): Expression =
    expression.transformExpressions {
      case expr if SuspendedArguments.representsSuspended(expr) => expr
      case n: Name.Literal =>
        if (typeParams.exists(_.name == n.name)) {
          n
        } else {
          processResolvedName(n, bindingsMap.resolveName(n.name))
        }
      case n: Name.Qualified =>
        processResolvedName(
          n,
          bindingsMap.resolveQualifiedName(n.parts.map(_.name))
        )
      case selfRef: Name.SelfType =>
        val resolvedSelfType = selfType.toRight {
          BindingsMap.SelfTypeOutsideOfTypeDefinition
        }
        processResolvedName(selfRef, resolvedSelfType)
      case s: `type`.Set =>
        s.mapExpressions(resolveSignature(selfType, typeParams, bindingsMap, _))
    }

  private def processResolvedName(
    name: Name,
    resolvedName: Either[BindingsMap.ResolutionError, BindingsMap.ResolvedName]
  ): Name =
    resolvedName
      .map(res => name.updateMetadata(new MetadataPair(this, Resolution(res))))
      .fold(
        error =>
          errors.Resolution(name, errors.Resolution.ResolverError(error)),
        n =>
          n.getMetadata(this).get.target match {
            case _: ResolvedModule =>
              errors.Resolution(
                n,
                errors.Resolution.UnexpectedModule("type signature")
              )
            case _ => n
          }
      )

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
    ir
  }

}
