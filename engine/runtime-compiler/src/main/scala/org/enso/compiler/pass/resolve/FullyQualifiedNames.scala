package org.enso.compiler.pass.resolve

import org.enso.compiler.PackageRepository
import org.enso.compiler.context.{CompilerContext, FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{Expression, Module, Name, Type}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.Export
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.expression.warnings
import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{ExportedModule, ModuleReference, Resolution, ResolvedType}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.core.Implicits.{AsDiagnostics, AsMetadata}
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{AliasAnalysis, BindingAnalysis}
import org.enso.compiler.pass.desugar.Imports
import org.enso.editions.LibraryName
import org.enso.pkg.QualifiedName

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
  override lazy val precursorPasses: Seq[IRPass] =
    Seq(AliasAnalysis, BindingAnalysis)

  /** The passes that are invalidated by running this pass. */
  override lazy val invalidatedPasses: Seq[IRPass] = Nil

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
    if (isMainModule(moduleContext)) {
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
                  case m: Export.Module
                      if m.name.name == resolution.qualifiedName.toString =>
                    m.addDiagnostic(
                      warnings.Shadowed.TypeInModuleNameConflicts(
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

  private def isMainModule(module: ModuleContext): Boolean = {
    module
      .getName()
      .item == Imports.mainModuleName.name && module.getName().path.length == 2
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
    processExpression(
      ir,
      scopeMap,
      List(),
      freshNameSupply,
      None,
      inlineContext.pkgRepo
    )

  }

  private def processModuleDefinition(
    moduleDefinition: Definition,
    bindings: BindingsMap,
    freshNameSupply: FreshNameSupply,
    pkgRepo: Option[PackageRepository]
  ): Definition = {
    moduleDefinition match {
      case asc: Type.Ascription => asc
      case method: definition.Method =>
        val resolution = method.methodReference.typePointer.flatMap(
          _.getMetadata(MethodDefinitions)
        )
        method.mapExpressions(
          processExpression(
            _,
            bindings,
            List(),
            freshNameSupply,
            resolution,
            pkgRepo
          )
        )
      case tp: Definition.Type =>
        tp.copy(members =
          tp.members.map(
            _.mapExpressions(
              processExpression(
                _,
                bindings,
                tp.params.map(_.name),
                freshNameSupply,
                bindings.resolveName(tp.name.name).toOption.map(Resolution),
                pkgRepo
              )
            )
          )
        )

      case a =>
        a.mapExpressions(
          processExpression(_, bindings, List(), freshNameSupply, None, pkgRepo)
        )
    }
  }

  private def processExpression(
    ir: Expression,
    bindings: BindingsMap,
    typeParams: List[Name],
    freshNameSupply: FreshNameSupply,
    selfTypeResolution: Option[Resolution],
    pkgRepo: Option[PackageRepository]
  ): Expression =
    ir.transformExpressions {
      case lit: Name.Literal =>
        val isTypeName = typeParams.find(_.name == lit.name).nonEmpty
        if (!lit.isMethod && !isLocalVar(lit) && !isTypeName) {
          val resolution = bindings.resolveName(lit.name)
          resolution match {
            case Left(_) =>
              if (
                pkgRepo
                  .map(_.isNamespaceRegistered(lit.name))
                  .getOrElse(false)
              ) {
                lit.updateMetadata(
                  new MetadataPair(
                    this,
                    FQNResolution(ResolvedLibraryNamespace(lit.name))
                  )
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
      case app @ Application.Prefix(_, List(_), _, _, _, _) =>
        app.function match {
          case lit: Name.Literal =>
            if (lit.isMethod)
              resolveLocalApplication(
                app,
                bindings,
                typeParams,
                freshNameSupply,
                pkgRepo,
                selfTypeResolution
              )
            else
              app.mapExpressions(
                processExpression(
                  _,
                  bindings,
                  typeParams,
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
                typeParams,
                freshNameSupply,
                selfTypeResolution,
                pkgRepo
              )
            )

        }

    }

  private def resolveLocalApplication(
    app: Application.Prefix,
    bindings: BindingsMap,
    typeParams: List[Name],
    freshNameSupply: FreshNameSupply,
    pkgRepo: Option[PackageRepository],
    selfTypeResolution: Option[Resolution]
  ): Expression = {
    val processedFun =
      processExpression(
        app.function,
        bindings,
        typeParams,
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
            typeParams,
            freshNameSupply,
            selfTypeResolution,
            pkgRepo
          )
        )
      )

    val processedApp: Option[Expression] = processedArgs match {
      case List(thisArg) =>
        val thisArgMeta = thisArg.value.getMetadata(this).map(_.target)
        // First, collect the name of the library and the module within that library
        // that should be resolve. These will only be present if the currently processed
        // IR has attached metadata from previous processing.
        (thisArgMeta, processedFun) match {
          case (Some(ResolvedLibraryNamespace(namespace)), name: Name.Literal) =>
            val libName = LibraryName(namespace, name.name)
            val modNameToResolve = QualifiedName(
              List(libName.namespace, libName.name),
              Imports.mainModuleName.name
            )
            transformLiteral(name, libName, modNameToResolve, pkgRepo, freshNameSupply)
          case (Some(ResolvedModule(moduleRef, Some(libName))), name: Name.Literal) =>
            // resolvedMod is a resolved module inside some library. Let's see whether
            // the current `name` literal points to a child module of the resolvedMod.
            val isMainModule = moduleRef.getName.item == Imports.mainModuleName.name
            val modNameToResolve = if (isMainModule) {
              QualifiedName(moduleRef.getName.path, name.name)
            } else {
              val newPath = moduleRef.getName.path ++ List(moduleRef.getName.item)
              QualifiedName(newPath, name.name)
            }
            transformLiteral(name, libName, modNameToResolve, pkgRepo, freshNameSupply)
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

  /**
   * Tries to resolve a FQN of a module within a library.
   * @param libName Library name of the library in which the module is to be resolved.
   * @param modFQN Fully qualified name of the module to be resolved.
   * @param literal Currently processed Literal
   * @param optPkgRepo Optional package repository. Not present in inline compilation.
   * @return
   */
  private def resolveQualName(
    libName: LibraryName,
    modFQN: QualifiedName,
    literal: Name.Literal,
    optPkgRepo: Option[PackageRepository]
  ): Either[Expression, Option[FQNResolution]] = {
    optPkgRepo.flatMap{ pkgRepo =>
      if (pkgRepo.isPackageLoaded(libName)) {
        pkgRepo
          .getLoadedModule(modFQN.toString)
          .map { loadedModule =>
            if (loadedModule.getIr == null && !loadedModule.isSynthetic) {
              // Limitation of Fully Qualified Names:
              // If the library has not been imported explicitly, then we won't have
              // IR for it. Triggering a full compilation at this stage may have
              // undesired consequences and is therefore prohibited on purpose.
              Left(
                errors.Resolution(
                  literal,
                  errors.Resolution
                    .MissingLibraryImportInFQNError(libName.namespace)
                )
              )
            } else {
              Right(
                Some(
                  FQNResolution(
                    ResolvedModule(
                      ModuleReference.Concrete(loadedModule),
                      Some(libName)
                    )
                  )
                )
              )
            }
          }
      } else {
        Some(
          Left(
            errors.Resolution(
              literal,
              errors.Resolution
                .MissingLibraryImportInFQNError(libName.namespace)
            )
          )
        )
      }
    }
    .getOrElse(Right(None))
  }

  /**
   * Optionally transforms the given literal to an IR error, or to a new literal
   * with attached metadata.
   * @param literal Literal to transform
   * @param libName Library name of the library in which the module is to be resolved.
   * @param modNameToResolve Fully qualified name of the module to be resolved.
   * @param pkgRepo Optional package repository. Not present in inline compilation.
   * @param freshNameSupply Fresh name supply for generating new names.
   * @return Either an IR error or a new literal with attached metadata.
   */
  private def transformLiteral(
    literal: Name.Literal,
    libName: LibraryName,
    modNameToResolve: QualifiedName,
    pkgRepo: Option[PackageRepository],
    freshNameSupply: FreshNameSupply
  ): Option[Expression] = {
    resolveQualName(
      libName,
      modNameToResolve,
      literal,
      pkgRepo
    ).fold(
      err => Some(err),
      _.map(resolvedMod =>
        freshNameSupply
          .newName(from = Some(literal))
          .updateMetadata(new MetadataPair(this, resolvedMod))
          .setLocation(literal.location)
      )
    )
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
    def prepareForSerialization(compiler: CompilerContext): PartiallyResolvedFQN
    def restoreFromSerialization(
      compiler: CompilerContext
    ): Option[PartiallyResolvedFQN]
  }

  case class ResolvedLibraryNamespace(namespace: String) extends PartiallyResolvedFQN {
    override def prepareForSerialization(
      compiler: CompilerContext
    ): PartiallyResolvedFQN = this

    override def restoreFromSerialization(
      compiler: CompilerContext
    ): Option[PartiallyResolvedFQN] = Some(this)
  }

  /**
   * @param moduleRef Reference to the resolved module
   * @param libName Optional name of the library that contains the module
   */
  case class ResolvedModule(
    moduleRef: ModuleReference,
    libName: Option[LibraryName]
  ) extends PartiallyResolvedFQN {
    override def prepareForSerialization(
      compiler: CompilerContext
    ): PartiallyResolvedFQN =
      ResolvedModule(moduleRef.toAbstract, libName)

    override def restoreFromSerialization(
      compiler: CompilerContext
    ): Option[PartiallyResolvedFQN] = {
      val packageRepository = compiler.getPackageRepository
      moduleRef
        .toConcrete(packageRepository.getModuleMap)
        .map(concreteMod => {
          ResolvedModule(concreteMod, libName)
        })
    }
  }

}
