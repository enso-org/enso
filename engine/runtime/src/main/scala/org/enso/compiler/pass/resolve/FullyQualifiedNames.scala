package org.enso.compiler.pass.resolve

import org.enso.compiler.{Compiler, PackageRepository}
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Error.Resolution.MissingLibraryImportInFQNError
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{
  ExportedModule,
  ModuleReference,
  Resolution,
  ResolvedType
}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{AliasAnalysis, BindingAnalysis}
import org.enso.compiler.pass.desugar.Imports
import org.enso.editions.LibraryName
import org.enso.interpreter.runtime.Module

/** Partially resolves fully qualified names corresponding to the library names
  *
  * 1. Identifies potential library names e.g., `Standard.Base`
  * 2. If the component has not been compiled yet, compilation is triggered
  * 3. Replaces the library name with a fresh name and a resolved Main module
  */
case object FullyQualifiedNames extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = FullyQualifiedNames.FQNResolution

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] =
    Seq(AliasAnalysis, BindingAnalysis)

  /** The passes that are invalidated by running this pass. */
  override val invalidatedPasses: Seq[IRPass] = Nil

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
    val new_bindings =
      ir.bindings.map(
        processModuleDefinition(
          _,
          scopeMap,
          freshNameSupply,
          moduleContext.pkgRepo
        )
      )

    // Detect potential name conflicts of exported types in Main modules
    // when using fully qualified names.
    // Consider that `Standard.Base.Main` exports `Standard.Base.Error.Error`
    // and module `Standard.Base.Error` defines `Error` type in it.
    // If there exists a module in that namespace, like
    // `Standard.Base.Error.Foo`, then accessing it via a fully qualified name,
    // `Standard.Base.Error.Foo`, will always lead to name conflicts with
    // the exported type `Error`.
    if (isMainModule(moduleContext.module)) {
      scopeMap.resolvedExports.foreach {
        case ExportedModule(
              resolution @ ResolvedType(exportedModuleRef, tpe),
              exportedAs,
              _
            ) =>
          val tpeName        = exportedAs.getOrElse(tpe.name)
          val exportedModule = exportedModuleRef.unsafeAsModule()
          if (
            exportedModuleRef.getName.path.length == 2 && exportedModuleRef.getName.item == tpeName && !exportedModule.isSynthetic
          ) {
            val allStarting = moduleContext.pkgRepo
              .map(
                _.getLoadedModules.filter(m =>
                  exportedModuleRef.getName != m.getName && m
                    .getName()
                    .toString
                    .startsWith(exportedModuleRef.getName.toString + ".")
                )
              )
              .getOrElse(Nil)
            if (allStarting.nonEmpty) {
              ir.exports.foreach { export =>
                export match {
                  case m: IR.Module.Scope.Export.Module
                      if m.name.name == resolution.qualifiedName.toString =>
                    m.addDiagnostic(
                      IR.Warning.Shadowed.TypeInModuleNameConflicts(
                        exportedModule.getName.toString,
                        tpeName,
                        allStarting.head.getName.toString,
                        m,
                        m.location
                      )
                    )
                  case _ =>
                }
              }
            }
          }
        case _ =>
      }
    }
    ir.copy(bindings = new_bindings)
  }

  private def isMainModule(module: Module): Boolean = {
    module.getName.item == Imports.mainModuleName.name && module.getName.path.length == 2
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
    processExpression(
      ir,
      scopeMap,
      freshNameSupply,
      None,
      inlineContext.pkgRepo
    )

  }

  private def processModuleDefinition(
    definition: IR.Module.Scope.Definition,
    bindings: BindingsMap,
    freshNameSupply: FreshNameSupply,
    pkgRepo: Option[PackageRepository]
  ): IR.Module.Scope.Definition = {
    definition match {
      case asc: IR.Type.Ascription => asc
      case method: IR.Module.Scope.Definition.Method =>
        val resolution = method.methodReference.typePointer.flatMap(
          _.getMetadata(MethodDefinitions)
        )
        method.mapExpressions(
          processExpression(_, bindings, freshNameSupply, resolution, pkgRepo)
        )
      case tp: IR.Module.Scope.Definition.Type =>
        tp.copy(members =
          tp.members.map(
            _.mapExpressions(
              processExpression(
                _,
                bindings,
                freshNameSupply,
                bindings.resolveName(tp.name.name).toOption.map(Resolution),
                pkgRepo
              )
            )
          )
        )

      case a =>
        a.mapExpressions(
          processExpression(_, bindings, freshNameSupply, None, pkgRepo)
        )
    }
  }

  private def processExpression(
    ir: IR.Expression,
    bindings: BindingsMap,
    freshNameSupply: FreshNameSupply,
    selfTypeResolution: Option[Resolution],
    pkgRepo: Option[PackageRepository]
  ): IR.Expression =
    ir.transformExpressions {
      case lit: IR.Name.Literal =>
        if (!lit.isMethod && !isLocalVar(lit)) {
          val resolution = bindings.resolveName(lit.name)
          resolution match {
            case Left(_) =>
              if (
                pkgRepo
                  .map(_.isNamespaceRegistered(lit.name))
                  .getOrElse(false)
              ) {
                lit.updateMetadata(
                  this -->> FQNResolution(ResolvedLibrary(lit.name))
                )
              } else {
                lit
              }
            case Right(_) =>
              lit
          }
        } else {
          lit
        }
      case app @ IR.Application.Prefix(_, List(_), _, _, _, _) =>
        app.function match {
          case lit: IR.Name.Literal =>
            if (lit.isMethod)
              resolveLocalApplication(
                app,
                bindings,
                freshNameSupply,
                pkgRepo,
                selfTypeResolution
              )
            else
              app.mapExpressions(
                processExpression(
                  _,
                  bindings,
                  freshNameSupply,
                  selfTypeResolution,
                  pkgRepo
                )
              )
          case _ =>
            app.mapExpressions(
              processExpression(
                _,
                bindings,
                freshNameSupply,
                selfTypeResolution,
                pkgRepo
              )
            )

        }

    }

  private def resolveLocalApplication(
    app: IR.Application.Prefix,
    bindings: BindingsMap,
    freshNameSupply: FreshNameSupply,
    pkgRepo: Option[PackageRepository],
    selfTypeResolution: Option[Resolution]
  ): IR.Expression = {
    val processedFun =
      processExpression(
        app.function,
        bindings,
        freshNameSupply,
        selfTypeResolution,
        pkgRepo
      )
    val processedArgs =
      app.arguments.map(
        _.mapExpressions(
          processExpression(
            _,
            bindings,
            freshNameSupply,
            selfTypeResolution,
            pkgRepo
          )
        )
      )

    val processedApp = processedArgs match {
      case List(thisArg) =>
        (thisArg.value.getMetadata(this).map(_.target), processedFun) match {
          case (Some(resolved @ ResolvedLibrary(_)), name: IR.Name.Literal) =>
            resolveQualName(resolved, name, pkgRepo).fold(
              err => Some(err),
              _.map(resolvedMod =>
                freshNameSupply
                  .newName()
                  .updateMetadata(this -->> resolvedMod)
                  .setLocation(name.location)
              )
            )
          case _ =>
            None
        }
      case _ =>
        None
    }

    processedApp.getOrElse(
      app.copy(function = processedFun, arguments = processedArgs)
    )
  }

  private def resolveQualName(
    thisResolution: ResolvedLibrary,
    consName: IR.Name.Literal,
    optPkgRepo: Option[PackageRepository]
  ): Either[IR.Expression, Option[FQNResolution]] = {
    optPkgRepo
      .flatMap { pkgRepo =>
        val libName = LibraryName(thisResolution.namespace, consName.name)
        if (pkgRepo.isPackageLoaded(libName)) {
          pkgRepo
            .getLoadedModule(
              s"${libName.toString}.${Imports.mainModuleName.name}"
            )
            .map { m =>
              if (m.getIr == null) {
                // Limitation of Fully Qualified Names:
                // If the library has not been imported explicitly, then we won't have
                // IR for it. Triggering a full compilation at this stage may have
                // undesired consequences and is therefore prohibited on purpose.
                Left(
                  IR.Error.Resolution(
                    consName,
                    MissingLibraryImportInFQNError(thisResolution.namespace)
                  )
                )
              } else {
                Right(
                  Some(
                    FQNResolution(ResolvedModule(ModuleReference.Concrete(m)))
                  )
                )
              }
            }
        } else {
          Some(
            Left(
              IR.Error.Resolution(
                consName,
                MissingLibraryImportInFQNError(thisResolution.namespace)
              )
            )
          )
        }
      }
      .getOrElse(Right(None))
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

  /** The FQN resolution metadata for a node.
    *
    * @param target the partially resolved name
    */
  sealed case class FQNResolution(target: PartiallyResolvedFQN)
      extends IRPass.IRMetadata {

    override val metadataName: String =
      "FullyQualifiedNames.Resolution"

    /** @inheritdoc */
    override def prepareForSerialization(compiler: Compiler): FQNResolution =
      FQNResolution(target.prepareForSerialization(compiler))

    /** @inheritdoc */
    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[FQNResolution] =
      target.restoreFromSerialization(compiler).map(FQNResolution)

    /** @inheritdoc */
    override def duplicate(): Option[IRPass.IRMetadata] = Some(this)
  }

  sealed trait PartiallyResolvedFQN {
    def prepareForSerialization(compiler: Compiler): PartiallyResolvedFQN
    def restoreFromSerialization(
      compiler: Compiler
    ): Option[PartiallyResolvedFQN]
  }

  case class ResolvedLibrary(namespace: String) extends PartiallyResolvedFQN {
    override def prepareForSerialization(
      compiler: Compiler
    ): PartiallyResolvedFQN = this

    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[PartiallyResolvedFQN] = Some(this)
  }
  case class ResolvedModule(moduleRef: ModuleReference)
      extends PartiallyResolvedFQN {
    override def prepareForSerialization(
      compiler: Compiler
    ): PartiallyResolvedFQN =
      ResolvedModule(moduleRef.toAbstract)

    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[PartiallyResolvedFQN] = {
      val packageRepository = compiler.context.getPackageRepository
      moduleRef
        .toConcrete(packageRepository.getModuleMap)
        .map(ResolvedModule(_))
    }
  }

}
