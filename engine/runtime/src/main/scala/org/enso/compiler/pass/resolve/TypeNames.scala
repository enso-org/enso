package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{Expression, Function, Module, Name}
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.MetadataStorage.ToPair
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
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    val bindingsMap =
      ir.unsafeGetMetadata(BindingAnalysis, "bindings analysis did not run")
    ir.copy(bindings = ir.bindings.map { d =>
      val mapped = d.mapExpressions(resolveExpression(bindingsMap, _))
      doResolveType(
        Nil,
        bindingsMap,
        mapped match {
          case typ: Definition.Type =>
            typ.members.foreach(m =>
              m.arguments.foreach(a =>
                doResolveType(typ.params.map(_.name), bindingsMap, a)
              )
            )
            typ
          case x => x
        }
      )
    })
  }

  private def resolveExpression(
    bindingsMap: BindingsMap,
    ir: Expression
  ): Expression = {
    def go(ir: Expression): Expression = {
      val processedIr = ir match {
        case fn: Function.Lambda =>
          fn.copy(arguments =
            fn.arguments.map(doResolveType(Nil, bindingsMap, _))
          )
        case x => x
      }
      doResolveType(Nil, bindingsMap, processedIr.mapExpressions(go))
    }
    go(ir)
  }

  private def doResolveType[T <: IR](
    typeParams: List[Name],
    bindingsMap: BindingsMap,
    ir: T
  ): T = {
    ir.getMetadata(TypeSignatures)
      .map { s =>
        ir.updateMetadata(
          TypeSignatures -->> TypeSignatures.Signature(
            resolveSignature(typeParams, bindingsMap, s.signature)
          )
        )
      }
      .getOrElse(ir)
  }

  private def resolveSignature(
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
      case s: `type`.Set =>
        s.mapExpressions(resolveSignature(typeParams, bindingsMap, _))
    }

  private def processResolvedName(
    name: Name,
    resolvedName: Either[BindingsMap.ResolutionError, BindingsMap.ResolvedName]
  ): Name =
    resolvedName
      .map(res => name.updateMetadata(this -->> Resolution(res)))
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
