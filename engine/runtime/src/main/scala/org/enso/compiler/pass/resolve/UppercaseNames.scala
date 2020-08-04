package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.ResolvedConstructor
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{AliasAnalysis, BindingAnalysis}
import org.enso.interpreter.Constants

case object UppercaseNames extends IRPass {

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
        "No fresh name supply passed to UppercaseNames resolver."
      )
    )
    ir.mapExpressions(processExpression(_, scopeMap, freshNameSupply))
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
        "No fresh name supply passed to UppercaseNames resolver."
      )
    )
    processExpression(ir, scopeMap, freshNameSupply)
  }

  private def processExpression(
    ir: IR.Expression,
    bindings: BindingsMap,
    freshNameSupply: FreshNameSupply
  ): IR.Expression =
    ir.transformExpressions {
      case lit: IR.Name.Literal =>
        if (lit.isReferant && !isLocalVar(lit)) {
          val resolution = bindings.resolveUppercaseName(lit.name)
          resolution match {
            case Left(error) =>
              IR.Error.Resolution(
                lit,
                IR.Error.Resolution.ResolverError(error)
              )
            case Right(value) =>
              lit.updateMetadata(this -->> BindingsMap.Resolution(value))
          }

        } else { lit }
      case app: IR.Application.Prefix =>
        val processedFun =
          processExpression(app.function, bindings, freshNameSupply)
        val processedArgs =
          app.arguments.map(
            _.mapExpressions(processExpression(_, bindings, freshNameSupply))
          )
        val newApp: Option[IR.Expression] = for {
          thisArgPos <- findThisPosition(processedArgs)
          thisArg = processedArgs(thisArgPos)
          thisArgResolution <- thisArg.value.getMetadata(this)
          funAsVar          <- asGlobalVar(processedFun)
          cons              <- resolveToCons(thisArgResolution, funAsVar)
          newFun =
            buildSymbolFor(cons, freshNameSupply).setLocation(funAsVar.location)
          newArgs = processedArgs.patch(thisArgPos, Nil, 1)
        } yield buildConsApplication(app, cons.cons, newFun, newArgs)
        newApp.getOrElse(
          app.copy(function = processedFun, arguments = processedArgs)
        )
    }

  private def buildConsApplication(
    originalApp: IR.Application.Prefix,
    calledCons: BindingsMap.Cons,
    newFun: IR.Expression,
    newArgs: List[IR.CallArgument]
  ): IR.Expression = {
    if (
      newArgs.isEmpty && (!originalApp.hasDefaultsSuspended || calledCons.arity == 0)
    ) {
      newFun
    } else {
      originalApp.copy(function = newFun, arguments = newArgs)
    }
  }

  private def buildSymbolFor(
    cons: BindingsMap.ResolvedConstructor,
    freshNameSupply: FreshNameSupply
  ): IR.Expression = {
    freshNameSupply
      .newReferantName()
      .updateMetadata(this -->> BindingsMap.Resolution(cons))
  }

  private def resolveToCons(
    thisResolution: BindingsMap.Resolution,
    consName: IR.Name.Literal
  ): Option[BindingsMap.ResolvedConstructor] =
    thisResolution.target match {
      case BindingsMap.ResolvedModule(module) =>
        val resolution = module.getIr
          .unsafeGetMetadata(
            BindingAnalysis,
            "Imported module without bindings analysis results"
          )
          .resolveExportedName(consName.name)
        resolution match {
          case Right(cons @ ResolvedConstructor(_, _)) => Some(cons)
          case _                                       => None
        }
      case _ => None
    }

  private def findThisPosition(args: List[IR.CallArgument]): Option[Int] = {
    val ix = args.indexWhere(arg =>
      arg.name.exists(
        _.name == Constants.Names.THIS_ARGUMENT
      ) || arg.name.isEmpty
    )
    if (ix == -1) None else Some(ix)
  }

  private def asGlobalVar(ir: IR): Option[IR.Name.Literal] =
    ir match {
      case name: IR.Name.Literal =>
        if (isLocalVar(name) || name.isReferant) None else Some(name)
      case _ => None
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
