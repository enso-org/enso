package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition.Method
import org.enso.compiler.core.ir.{`type`, Expression, Function, Module, Name}
import org.enso.compiler.core.{CompilerError, IR}
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
      val selfTypeInfo: SelfTypeInfo = d match {
        case t: Definition.Type => SelfTypeInfo.fromTypeDefinition(t)
        case m: Method.Explicit =>
          SelfTypeInfo.fromMethodReference(m.methodReference)
        case _ => SelfTypeInfo.empty
      }

      val mapped =
        d.mapExpressions(
          resolveExpression(selfTypeInfo, bindingsMap, _)
        )
      val withResolvedArguments = mapped match {
        case typ: Definition.Type =>
          typ.members.foreach(m =>
            m.arguments.foreach(a =>
              doResolveType(
                SelfTypeInfo.fromTypeDefinition(typ),
                bindingsMap,
                a
              )
            )
          )
          typ
        case x => x
      }
      doResolveType(selfTypeInfo, bindingsMap, withResolvedArguments)
    })
  }

  private case class SelfTypeInfo(
    selfType: Option[BindingsMap.ResolvedType],
    typeParams: List[Name]
  )

  private case object SelfTypeInfo {
    def empty: SelfTypeInfo = SelfTypeInfo(None, Nil)

    def fromTypeDefinition(d: Definition.Type): SelfTypeInfo = {
      // TODO currently the `Self` type is only used internally as an ascription for static method bindings
      //  Once we actually start supporting the `Self` syntax, we should set the self type here to the ResolvedType
      //  corresponding to the current definition, so that we can correctly resolve `Self` references in constructor
      //  argument types.
      val selfType   = None
      val typeParams = d.params.map(_.name)
      SelfTypeInfo(selfType, typeParams)
    }

    def fromMethodReference(m: Name.MethodReference): SelfTypeInfo =
      m.typePointer match {
        case Some(p) =>
          p.getMetadata(MethodDefinitions) match {
            case Some(resolution) =>
              resolution.target match {
                case typ: BindingsMap.ResolvedType =>
                  val params =
                    typ.tp.params
                      .map(Name.Literal(_, false, identifiedLocation = null))
                      .toList
                  SelfTypeInfo(Some(typ), params)
                case _: BindingsMap.ResolvedModule =>
                  SelfTypeInfo.empty
                case other =>
                  throw new CompilerError(
                    s"Method target not resolved as ResolvedType, but $other."
                  )
              }
            case None =>
              // It is unexpected that the metadata is missing here, but we don't fail because other passes should fail
              // with more detailed info.
              SelfTypeInfo.empty
          }
        case None => SelfTypeInfo.empty
      }
  }

  private def resolveExpression(
    selfTypeInfo: SelfTypeInfo,
    bindingsMap: BindingsMap,
    ir: Expression
  ): Expression = {
    def go(ir: Expression): Expression = {
      val processedIr = ir match {
        case fn: Function.Lambda =>
          fn.copy(arguments =
            fn.arguments.map(
              doResolveType(selfTypeInfo, bindingsMap, _)
            )
          )
        case x => x
      }
      doResolveType(
        selfTypeInfo,
        bindingsMap,
        processedIr.mapExpressions(go)
      )
    }
    go(ir)
  }

  private def doResolveType[T <: IR](
    selfTypeInfo: SelfTypeInfo,
    bindingsMap: BindingsMap,
    ir: T
  ): T = {
    ir.getMetadata(TypeSignatures)
      .map { s =>
        ir.updateMetadata(
          new MetadataPair(
            TypeSignatures,
            TypeSignatures.Signature(
              resolveSignature(selfTypeInfo, bindingsMap, s.signature),
              s.comment
            )
          )
        )
      }
      .getOrElse(ir)
  }

  private def resolveSignature(
    selfTypeInfo: SelfTypeInfo,
    bindingsMap: BindingsMap,
    expression: Expression
  ): Expression =
    expression.transformExpressions {
      case expr if SuspendedArguments.representsSuspended(expr) => expr
      case n: Name.Literal =>
        if (selfTypeInfo.typeParams.exists(_.name == n.name)) {
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
        val resolvedSelfType = selfTypeInfo.selfType match {
          case None =>
            Left(BindingsMap.SelfTypeOutsideOfTypeDefinition)
          case Some(selfType) =>
            Right(List(selfType))
        }
        processResolvedName(selfRef, resolvedSelfType)
      case s: `type`.Set =>
        s.mapExpressions(resolveSignature(selfTypeInfo, bindingsMap, _))
    }

  private def processResolvedName(
    name: Name,
    resolvedNamesOpt: Either[BindingsMap.ResolutionError, List[
      BindingsMap.ResolvedName
    ]]
  ): Name =
    resolvedNamesOpt
      .map(resolvedNames => {
        resolvedNames.foreach { resolvedName =>
          name.updateMetadata(new MetadataPair(this, Resolution(resolvedName)))
        }
        name
      })
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
