package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Expression,
  Module,
  Name,
  Type
}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{
  Resolution,
  ResolutionNotFound,
  ResolvedMethod,
  ResolvedModule
}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{AliasAnalysis, BindingAnalysis}
import org.enso.interpreter.Constants

/** Resolves name occurences in non-pattern contexts.
  *
  * 1. Attaches resolution metadata to encountered constructors, modules,
  *    and polygot symbols.
  * 2. Desugars encountered method references into proper applications.
  * 3. Resolves qualified calls to constructors, i.e. a call of the form
  *    `KnownModule.consName a b c` is transformed into `KnownCons a b c`,
  *    if `consName` refers to a constructor and `KnownModule` was successfully
  *    resolved to a module.
  */
case object GlobalNames extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = BindingsMap.Resolution

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] =
    Seq(AliasAnalysis, BindingAnalysis, FullyQualifiedNames)

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
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    val scopeMap = ir.unsafeGetMetadata(
      BindingAnalysis,
      "No binding analysis on the module"
    )
    val freshNameSupply = moduleContext.freshNameSupply.getOrElse(
      throw new CompilerError(
        "No fresh name supply passed to GlobalNames resolver."
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
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
    val scopeMap = inlineContext.bindingsAnalysis()
    val freshNameSupply = inlineContext.freshNameSupply.getOrElse(
      throw new CompilerError(
        "No fresh name supply passed to UppercaseNames resolver."
      )
    )
    processExpression(ir, scopeMap, List(), freshNameSupply, None)
  }

  /** @inheritdoc */

  private def processModuleDefinition(
    moduleDefinition: Definition,
    bindings: BindingsMap,
    freshNameSupply: FreshNameSupply
  ): Definition = {
    moduleDefinition match {
      case asc: Type.Ascription => asc
      case method: definition.Method =>
        val resolution = method.methodReference.typePointer.flatMap(
          _.getMetadata(MethodDefinitions)
        )
        method.mapExpressions(
          processExpression(_, bindings, List(), freshNameSupply, resolution)
        )
      case tp: Definition.Type =>
        tp.copy(members =
          tp.members.map(
            _.mapExpressions(
              processExpression(
                _,
                bindings,
                tp.params,
                freshNameSupply,
                bindings.resolveName(tp.name.name).toOption.map(Resolution)
              )
            )
          )
        )

      case a =>
        a.mapExpressions(
          processExpression(_, bindings, List(), freshNameSupply, None)
        )
    }
  }

  private def processExpression(
    ir: Expression,
    bindings: BindingsMap,
    params: List[DefinitionArgument],
    freshNameSupply: FreshNameSupply,
    selfTypeResolution: Option[Resolution],
    isInsideApplication: Boolean = false
  ): Expression = {
    ir.transformExpressions {
      case selfTp: Name.SelfType =>
        selfTypeResolution
          .map(res => selfTp.updateMetadata(this -->> res))
          .getOrElse(
            errors.Resolution(
              selfTp,
              errors.Resolution.ResolverError(ResolutionNotFound)
            )
          )
      case lit: Name.Literal =>
        if (params.exists(p => p.name.name == lit.name)) {
          lit
        } else {
          lit.getMetadata(FullyQualifiedNames) match {
            case Some(
                  FullyQualifiedNames.FQNResolution(
                    FullyQualifiedNames.ResolvedModule(modRef)
                  )
                ) =>
              lit.updateMetadata(this -->> Resolution(ResolvedModule(modRef)))
            case _ =>
              if (!lit.isMethod && !isLocalVar(lit)) {
                val resolution = bindings.resolveName(lit.name)
                resolution match {
                  case Left(error) =>
                    errors.Resolution(
                      lit,
                      errors.Resolution.ResolverError(error)
                    )
                  case Right(r @ BindingsMap.ResolvedMethod(mod, method)) =>
                    if (isInsideApplication) {
                      lit.updateMetadata(this -->> BindingsMap.Resolution(r))
                    } else {
                      val self = freshNameSupply
                        .newName()
                        .updateMetadata(
                          this -->> BindingsMap.Resolution(
                            BindingsMap.ResolvedModule(mod)
                          )
                        )
                      // The synthetic applications gets the location so that instrumentation
                      // identifies the node correctly
                      val fun = lit.copy(
                        name     = method.name,
                        location = None
                      )
                      val app = Application.Prefix(
                        fun,
                        List(CallArgument.Specified(None, self, None)),
                        hasDefaultsSuspended = false,
                        lit.location
                      )
                      fun
                        .getMetadata(ExpressionAnnotations)
                        .foreach(annotationsMeta =>
                          app.updateMetadata(
                            ExpressionAnnotations -->> annotationsMeta
                          )
                        )
                      fun.passData.remove(ExpressionAnnotations)
                      app
                    }
                  case Right(value) =>
                    lit.updateMetadata(this -->> BindingsMap.Resolution(value))
                }

              } else {
                lit
              }
          }
        }
      case app: Application.Prefix =>
        app.function match {
          case lit: Name.Literal =>
            if (!lit.isMethod)
              resolveReferantApplication(
                app,
                lit,
                bindings,
                params,
                freshNameSupply,
                selfTypeResolution
              )
            else
              resolveLocalApplication(
                app,
                bindings,
                params,
                freshNameSupply,
                selfTypeResolution
              )
          case _ =>
            app.mapExpressions(
              processExpression(
                _,
                bindings,
                params,
                freshNameSupply,
                selfTypeResolution
              )
            )

        }

    }
  }

  private def resolveReferantApplication(
    app: Application.Prefix,
    fun: Name.Literal,
    bindingsMap: BindingsMap,
    params: List[DefinitionArgument],
    freshNameSupply: FreshNameSupply,
    selfTypeResolution: Option[Resolution]
  ): Expression = {
    val processedFun = processExpression(
      app.function,
      bindingsMap,
      params,
      freshNameSupply,
      selfTypeResolution,
      isInsideApplication = true
    )
    val processedArgs = app.arguments.map(
      _.mapExpressions(
        processExpression(
          _,
          bindingsMap,
          params,
          freshNameSupply,
          selfTypeResolution
        )
      )
    )
    processedFun.getMetadata(this) match {
      case Some(Resolution(ResolvedMethod(mod, _))) if !isLocalVar(fun) =>
        val self = freshNameSupply
          .newName()
          .updateMetadata(
            this -->> BindingsMap.Resolution(
              BindingsMap.ResolvedModule(mod)
            )
          )
        val selfArg = CallArgument.Specified(None, self, None)
        processedFun.passData.remove(this) // Necessary for IrToTruffle
        app.copy(function = processedFun, arguments = selfArg :: processedArgs)
      case _ =>
        app.copy(function = processedFun, arguments = processedArgs)
    }
  }

  private def resolveLocalApplication(
    app: Application.Prefix,
    bindings: BindingsMap,
    params: List[DefinitionArgument],
    freshNameSupply: FreshNameSupply,
    selfTypeResolution: Option[Resolution]
  ): Expression = {
    val processedFun =
      processExpression(
        app.function,
        bindings,
        params,
        freshNameSupply,
        selfTypeResolution
      )
    val processedArgs =
      app.arguments.map(
        _.mapExpressions(
          processExpression(
            _,
            bindings,
            params,
            freshNameSupply,
            selfTypeResolution
          )
        )
      )

    val appData = for {
      thisArgPos <- findThisPosition(processedArgs)
      thisArg = processedArgs(thisArgPos)
      thisArgResolution <- thisArg.value.getMetadata(this)
      funAsVar          <- asGlobalVar(processedFun)
      cons              <- resolveQualName(thisArgResolution, funAsVar)
    } yield (thisArgPos, funAsVar, cons)

    val newApp = appData.flatMap {
      case (
            thisArgPos,
            funAsVar,
            cons: BindingsMap.ResolvedConstructor
          ) =>
        val newFun =
          buildSymbolFor(cons, freshNameSupply).setLocation(funAsVar.location)
        val newArgs = processedArgs.patch(thisArgPos, Nil, 1)
        Some(buildConsApplication(app, cons.cons, newFun, newArgs))
      case _ => None
    }
    newApp.getOrElse(
      app.copy(function = processedFun, arguments = processedArgs)
    )
  }

  private def buildConsApplication(
    originalApp: Application.Prefix,
    calledCons: BindingsMap.Cons,
    newFun: Expression,
    newArgs: List[CallArgument]
  ): Expression = {
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
  ): Expression = {
    freshNameSupply
      .newName()
      .updateMetadata(this -->> BindingsMap.Resolution(cons))
  }

  private def resolveQualName(
    thisResolution: BindingsMap.Resolution,
    consName: Name.Literal
  ): Option[BindingsMap.ResolvedName] =
    thisResolution.target match {
      case BindingsMap.ResolvedModule(module) =>
        val resolution = module
          .unsafeAsModule()
          .getIr
          .unsafeGetMetadata(
            BindingAnalysis,
            "Imported module without bindings analysis results"
          )
          .resolveExportedName(consName.name)
        resolution match {
          case Right(res) => Some(res)
          case _          => None
        }
      case _ => None
    }

  private def findThisPosition(args: List[CallArgument]): Option[Int] = {
    val ix = args.indexWhere(arg =>
      arg.name.exists(
        _.name == Constants.Names.SELF_ARGUMENT
      ) || arg.name.isEmpty
    )
    if (ix == -1) None else Some(ix)
  }

  private def asGlobalVar(ir: IR): Option[Name.Literal] =
    ir match {
      case name: Name.Literal =>
        if (isLocalVar(name)) None else Some(name)
      case _ => None
    }

  private def isLocalVar(name: Name.Literal): Boolean = {
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
