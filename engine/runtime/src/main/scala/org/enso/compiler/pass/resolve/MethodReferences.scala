package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{AliasAnalysis, BindingAnalysis}
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap.{Resolution, ResolvedMethod}

import scala.annotation.unused

case object MethodReferences extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = BindingsMap.Resolution

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] =
    Seq(AliasAnalysis, BindingAnalysis)

  /** The passes that are invalidated by running this pass. */
  override val invalidatedPasses: Seq[IRPass] = Seq(AliasAnalysis)

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
    val scopeMap = ir.unsafeGetMetadata(
      BindingAnalysis,
      "No binding analysis on the module"
    )
    val freshNameSupply = moduleContext.freshNameSupply.getOrElse(
      throw new CompilerError(
        "No fresh name supply passed to MethodReferences resolver."
      )
    )
    val new_bindings =
      ir.bindings.map(processModuleDefinition(_, scopeMap, freshNameSupply))
    ir.copy(bindings = new_bindings)
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
  ): IR.Expression = {
    val scopeMap = inlineContext.module.getIr.unsafeGetMetadata(
      BindingAnalysis,
      "No binding analysis on the module"
    )
    val freshNameSupply = inlineContext.freshNameSupply.getOrElse(
      throw new CompilerError(
        "No fresh name supply passed to MethodReferences resolver."
      )
    )
    processExpression(ir, scopeMap, freshNameSupply)
  }

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr

  private def processModuleDefinition(
    definition: IR.Module.Scope.Definition,
    bindings: BindingsMap,
    freshNameSupply: FreshNameSupply
  ): IR.Module.Scope.Definition = {
    definition match {
      case asc: IR.Type.Ascription => asc
      case a =>
        a.mapExpressions(processExpression(_, bindings, freshNameSupply))
    }
  }

  private def processExpression(
    ir: IR.Expression,
    bindings: BindingsMap,
    freshNameSupply: FreshNameSupply,
    isInsideApplication: Boolean = false
  ): IR.Expression =
    ir.transformExpressions {
      case lit: IR.Name.Literal if !lit.isMethod =>
        val resolution = bindings.resolveMethodName(lit.name)
        resolution match {
          case Right(r @ BindingsMap.ResolvedMethod(mod, method))
              if !isLocalVar(lit) =>
            if (isInsideApplication) {
              lit
                .updateMetadata(this -->> BindingsMap.Resolution(r))
                .copy(isMethod = true)
            } else {
              val self = freshNameSupply
                .newName(isReferent = false)
                .updateMetadata(
                  this -->> BindingsMap.Resolution(
                    BindingsMap.ResolvedModule(mod)
                  )
                )
              val fun = lit.copy(name = method.name, isMethod = true)
              val app = IR.Application.Prefix(
                fun,
                List(IR.CallArgument.Specified(None, self, None)),
                hasDefaultsSuspended = false,
                None
              )
              app
            }
          case _ =>
            // Don't report any errors in resolution.
            // They will be caught in later phases, if necessary.
            lit
        }

      case app: IR.Application.Prefix =>
        app.function match {
          case lit: IR.Name.Literal if !lit.isMethod =>
            resolveApplication(app, lit, bindings, freshNameSupply)
          case _ =>
            app.mapExpressions(processExpression(_, bindings, freshNameSupply))
        }
    }

  private def resolveApplication(
    app: IR.Application.Prefix,
    fun: IR.Name.Literal,
    bindingsMap: BindingsMap,
    freshNameSupply: FreshNameSupply
  ): IR.Expression = {
    val processedFun = processExpression(
      app.function,
      bindingsMap,
      freshNameSupply,
      isInsideApplication = true
    )
    val processedArgs = app.arguments.map(
      _.mapExpressions(processExpression(_, bindingsMap, freshNameSupply))
    )
    processedFun.getMetadata(this) match {
      case Some(Resolution(ResolvedMethod(mod, _))) if !isLocalVar(fun) =>
        val self = freshNameSupply
          .newName(isReferent = false)
          .updateMetadata(
            this -->> BindingsMap.Resolution(
              BindingsMap.ResolvedModule(mod)
            )
          )
        val selfArg = IR.CallArgument.Specified(None, self, None)
        processedFun.passData.remove(this) // Necessary for IrToTruffle
        app.copy(function = processedFun, arguments = selfArg :: processedArgs)
      case _ =>
        app.copy(function = processedFun, arguments = processedArgs)
    }
  }

  private def isLocalVar(name: IR.Name.Literal): Boolean = {
    val aliasInfo = name
      .unsafeGetMetadata(
        AliasAnalysis,
        "no alias analysis info on a name"
      )
      .unsafeAs[AliasAnalysis.Info.Occurrence]
    val defLink = aliasInfo.graph.defLinkFor(aliasInfo.id)
    defLink.isDefined
  }

}
