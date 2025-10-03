package org.enso.compiler.data

import org.enso.compiler.PackageRepository
import org.enso.compiler.PackageRepository.ModuleMap
import org.enso.compiler.context.CompilerContext.Module
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.{ir, CompilerError}
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.data.BindingsMap.{DefinedEntity, ModuleReference}
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.compiler.pass.resolve.MethodDefinitions
import org.enso.editions.LibraryName
import org.enso.pkg.{Config, QualifiedName}

import scala.collection.mutable.ArrayBuffer

/** A utility structure for resolving symbols in a given module.
  * This code is designated to be rewritten into Java. When making changes
  * consider moving more and more functionality to BindingsMapBase.
  */
final class BindingsMap private (initial: BindingsMapBase.State)
    extends BindingsMapBase(initial) {
  import BindingsMap._

  /** Constructor with entities and module reference.
    *
    * @param definedEntities the list of entities defined in the current module
    * @param currentModule the module holding these bindings
    */
  def this(de: List[DefinedEntity], cm: ModuleReference) =
    this(new BindingsMapBase.State(de, cm))

  override val metadataName: String = "Bindings Map"

  override def duplicate(): Option[IRPass.IRMetadata] = Some(this)

  def resolvedImports(v: List[ResolvedImport]): Unit = {
    updateState(_.withResolvedImports(v), false)
  }

  def exportedSymbols(v: Map[String, List[ResolvedName]]): Unit = {
    updateState(_.withExportedSymbols(v), false)
  }

  /** @inheritdoc */
  override def prepareForSerialization(
    compiler: Compiler
  ): BindingsMap = {
    this.toAbstract
  }

  /** @inheritdoc */
  override def restoreFromSerialization(
    compiler: Compiler
  ): Option[BindingsMap] = {
    val repo = compiler.getPackageRepository
    def ensureConvertedToConcrete(
      state: BindingsMapBase.State
    ): BindingsMapBase.State = {
      val opt = toConcrete(state, repo, repo.getModuleMap).flatMap { s =>
        val cm = s.currentModule
        val es = s.exportedSymbols
        val ri = s.resolvedImports
        Some(
          new BindingsMapBase.State(
            s.definedEntities,
            cm,
            ri,
            es
          )
        )
      }
      if (opt.isEmpty) {
        throw new java.io.IOException("Cannot deserialize " + this)
      } else {
        opt.get
      }
    }
    // lazily update state
    this.updateState(ensureConvertedToConcrete, true)
    Some(this)
  }

  /** Convert this [[BindingsMap]] instance to use abstract module references.
    *
    * @return `this` with module references converted to abstract
    */
  def toAbstract: BindingsMap = {
    val initial = getState()
    val cm      = initial.currentModule.toAbstract
    val ri      = initial.resolvedImports.map(_.toAbstract)
    val es = initial.exportedSymbols.map { case (key, value) =>
      key -> value.map(name => name.toAbstract)
    }
    val state = initial
      .withCurrentModule(cm)
      .withResolvedImports(ri)
      .withExportedSymbols(es)
    new BindingsMap(state)
  }

  private def toConcrete(
    state: BindingsMapBase.State,
    r: PackageRepository,
    moduleMap: ModuleMap
  ): Option[BindingsMapBase.State] = {
    val newMap = state.currentModule
      .toConcrete(moduleMap)
      .map { module =>
        state.withCurrentModule(module)
      }

    val withImports: Option[BindingsMapBase.State] = newMap.flatMap { s =>
      val newImports = s.resolvedImports.map { imp =>
        imp.targets.foreach { t =>
          t.toLibraryName.foreach(r.ensurePackageIsLoaded);
        }
        imp.toConcrete(moduleMap)
      }
      if (newImports.exists(_.isEmpty)) {
        None
      } else {
        val w = s.withResolvedImports(newImports.map(_.get))
        Some(w)
      }
    }

    val withSymbols: Option[BindingsMapBase.State] = withImports.flatMap { s =>
      val newSymbols =
        s.exportedSymbols.map { case (key, value) =>
          val newValue = value.map(_.toConcrete(moduleMap))
          if (newValue.exists(_.isEmpty)) {
            key -> None
          } else {
            key -> Some(newValue.map(_.get))
          }
        }

      if (newSymbols.exists { case (_, v) => v.isEmpty }) {
        None
      } else {
        val newValue = newSymbols.map { case (k, v) =>
          k -> v.get
        }
        val w = s.withExportedSymbols(newValue)
        Some(w)
      }
    }

    withSymbols
  }

  private def findLocalCandidates(name: String): List[ResolvedName] = {
    val candidates =
      definedEntities.filter(_.name == name).map(_.resolvedIn(currentModule))
    if (candidates.isEmpty && currentModule.getName.item == name) {
      List(ResolvedModule(currentModule))
    } else { candidates }
  }

  private def findQualifiedImportCandidates(
    name: String
  ): List[ResolvedName] = {
    resolvedImports
      .filter(i => importMatchesName(i, name) && !i.isSynthetic())
      .flatMap(_.targets)
  }

  private def importMatchesName(imp: ResolvedImport, name: String): Boolean = {
    imp.importDef.onlyNames
      .map(_ => imp.importDef.rename.exists(_.name == name))
      .getOrElse(
        !imp.importDef.isAll && imp.importDef.getSimpleName.name == name
      )
  }

  private def findExportedCandidatesInImports(
    name: String
  ): List[ResolvedName] = {

    val resolvedNames = resolvedImports
      .flatMap { imp =>
        if (imp.importDef.allowsAccess(name)) {
          imp
            .findExportedSymbolsFor(name)
            .map((_, imp.isSynthetic(), imp.targets))
        } else { List() }
      }
    // synthetic imports should not be reported in the ambiguity reports
    (resolvedNames match {
      case _ :: _ :: _ =>
        resolvedNames.filter(!_._2)
      case _ =>
        resolvedNames
    }).map(_._1)
  }

  /** Resolves the symbol with the given name in the context of this import target.
    * Note that it is valid to have multiple resolved names for a single symbol name,
    * for example, if the symbol is a name of an extension method and there are multiple
    * extension methods with the same name defined on multiple types.
    *
    * @param name (Unqualified) name of the symbol to resolve
    * @return A list of all resolutions for the given name or an error if no resolution
    *         was found
    */
  def resolveName(
    name: String
  ): Either[ResolutionError, List[ResolvedName]] = {
    val local = findLocalCandidates(name)
    if (local.nonEmpty) {
      return BindingsMap.handleAmbiguity(local)
    }
    val qualifiedImps = findQualifiedImportCandidates(name)
    if (qualifiedImps.nonEmpty) {
      return handleAmbiguity(qualifiedImps)
    }
    handleAmbiguity(
      findExportedCandidatesInImports(name)
    )
  }

  def resolveQualifiedNameIn(
    scope: ResolvedName,
    submoduleNames: List[String],
    finalItem: String
  ): Either[ResolutionError, List[ResolvedName]] = scope match {
    case scoped: ImportTarget =>
      var currentScope = scoped
      for (modName <- submoduleNames) {
        val resolutions = currentScope.resolveExportedSymbol(modName)
        resolutions match {
          case Left(err) => return Left(err)
          case Right(List(t: ImportTarget)) =>
            currentScope = t
          case _ => return Left(ResolutionNotFound)
        }
      }
      currentScope.resolveExportedSymbol(finalItem)
    case s @ ResolvedPolyglotSymbol(_, _) =>
      val found = s.findExportedSymbolFor(finalItem)
      Right(List(found))
    case _ => Left(ResolutionNotFound)
  }

  /** Resolves a qualified name to a symbol in the context of this module.
    * The name may be imported from a different project.
    *
    * @param name the name to resolve
    * @return a resolution for `name`
    * @see [[resolveName]]
    */
  def resolveQualifiedName(
    name: List[String]
  ): Either[ResolutionError, List[ResolvedName]] = {
    if (fqnHasNamespace(name)) {
      val resolution = resolveQualifiedNameFromDifferentProject(name)
      resolution match {
        case Right(resolved) => Right(resolved)
        case Left(_)         =>
          // If not found from different project, fallback to resolution from this project.
          resolveQualifiedNameFromThisProject(name)
      }
    } else {
      resolveQualifiedNameFromThisProject(name)
    }
  }

  private def resolveQualifiedNameFromThisProject(
    name: List[String]
  ): Either[ResolutionError, List[ResolvedName]] = {
    name match {
      case List()     => Left(ResolutionNotFound)
      case List(item) => resolveName(item)
      case firstModuleName :: rest =>
        val firstResolvedNamesOpt = resolveName(firstModuleName)
        firstResolvedNamesOpt match {
          case err @ Left(_)             => err
          case Right(firstResolvedNames) =>
            // This special handling is needed, because when resolving a local module name, we do not necessarily only look at _exported_ symbols, but overall locally defined symbols.
            val isQualifiedLocalImport =
              firstResolvedNames == List(ResolvedModule(currentModule))
            if (isQualifiedLocalImport) {
              resolveLocalName(rest)
            } else {
              val consName = rest.last
              val modNames = rest.init

              val allResolvedNames: ArrayBuffer[ResolvedName] =
                ArrayBuffer.empty
              firstResolvedNames.foreach { firstResolvedName =>
                val res =
                  resolveQualifiedNameIn(firstResolvedName, modNames, consName)
                res match {
                  case Left(resolutionErr) => return Left(resolutionErr)
                  case Right(resolved) =>
                    allResolvedNames ++= resolved
                }
              }
              Right(allResolvedNames.toList)
            }
        }
    }
  }

  /** Resolves a qualified name that is "absolute" - its first parts are namespace and project name.
    * This is a special case because we first need to decide whether the project is imported at all.
    * The name may be located in different project, hence the name.
    * @param name Fully qualified name, with at least 3 parts: namespace, project name, module name.
    * @param bindingsMapStack Stack of already visited bindings maps to avoid infinite recursion.
    * @return
    */
  private def resolveQualifiedNameFromDifferentProject(
    name: List[String],
    bindingsMapStack: Set[QualifiedName] = Set()
  ): Either[ResolutionError, List[ResolvedName]] = {
    assert(
      name.size > 2,
      "Expected to have at least namespace and project name"
    )
    val namespace = name(0)
    val projName  = name(1)
    if (shouldSearchInCurrentModule(name)) {
      return resolveName(name.last)
    }
    val matchingImportsFromProject = resolvedImports.flatMap { imp =>
      val hasMatchingTarget = imp.targets.exists { target =>
        isTargetFromProject(target, namespace, projName)
      }
      if (hasMatchingTarget) {
        Some(imp)
      } else {
        None
      }
    }
    val matchingModulesFromProject = matchingImportsFromProject.flatMap { imp =>
      imp.targets.map(_.module)
    }.distinct

    val modName = name(2)
    val matchingModules = matchingModulesFromProject.flatMap { mod =>
      val importedModName = mod.getName.item
      if (importedModName == modName) {
        Some(mod)
      } else {
        None
      }
    }
    if (matchingModules.isEmpty) {
      // Traverse BindingsMap of matchingModulesFromProject
      val matchingConcreteModules = matchingModulesFromProject.collect {
        case ModuleReference.Concrete(concreteMod) => concreteMod
      }
      val importedBindingMaps = matchingConcreteModules
        // Consider only modules that are not in `bindingsMapStack`
        .filterNot(mod => bindingsMapStack.contains(mod.getName))
        .map(_.getBindingsMap)
        // Avoid infinite loops with the identical binding maps
        .filterNot(_ eq this)
      importedBindingMaps.foreach { bm =>
        val bmName = bm.currentModule.getName
        assert(!bindingsMapStack.contains(bmName))
        val resolution = bm.resolveQualifiedNameFromDifferentProject(
          name,
          bindingsMapStack + bmName
        )
        resolution match {
          case Left(err)  => return Left(err)
          case Right(res) => return Right(res)
        }
      }
    }

    val restOfFQN                                 = name.drop(3)
    val allResolutions: ArrayBuffer[ResolvedName] = ArrayBuffer.empty
    matchingModules.foreach { mod =>
      val resolvedMod = ResolvedModule(mod)
      if (restOfFQN.nonEmpty) {
        val directResolutions =
          resolvedMod.findExportedSymbolsFor(restOfFQN.last)
        if (directResolutions.nonEmpty) {
          allResolutions.addAll(directResolutions)
        } else {
          // Try one more time with `resolveQualifiedNameIn`
          val resolution = resolveQualifiedNameIn(
            resolvedMod,
            restOfFQN.init,
            restOfFQN.last
          )
          resolution match {
            case Left(err) =>
              return Left(err)
            case Right(res) =>
              allResolutions.addAll(res)
          }
        }
      } else {
        if (mod.getName.item == modName) {
          allResolutions.addOne(
            resolvedMod
          )
        }
      }
    }
    if (allResolutions.isEmpty) {
      Left(ResolutionNotFound)
    } else {
      handleAmbiguity(allResolutions.toList)
    }
  }

  private def fqnHasNamespace(name: List[String]): Boolean = {
    name.size > 2 && (name.head == Config.DefaultNamespace || name.head == "Standard")
  }

  private def isTargetFromProject(
    importTarget: ImportTarget,
    namespace: String,
    projName: String
  ): Boolean = {
    val modName = importTarget.module.getName.fullPath()
    if (modName.size > 2) {
      modName(0) == namespace && modName(1) == projName
    } else {
      false
    }
  }

  /** Returns true iff the given fully qualified name should be resolved in current
    * module and no other imports should be considered.
    */
  private def shouldSearchInCurrentModule(
    name: List[String]
  ): Boolean = {
    val curModName = currentModule.getName
    if (curModName.item == "Main") {
      curModName.path == name.dropRight(1)
    } else {
      curModName.fullPath() == name.dropRight(1) ||
      curModName.fullPath() == name
    }
  }

  private def resolveLocalName(
    name: List[String]
  ): Either[ResolutionError, List[ResolvedName]] = name match {
    case List() => Left(ResolutionNotFound)
    case List(singleItem) =>
      handleAmbiguity(findLocalCandidates(singleItem))
    case firstName :: rest =>
      handleAmbiguity(findLocalCandidates(firstName))
        .flatMap(resolvedNames => {
          val allResolvedNames: ArrayBuffer[ResolvedName] = ArrayBuffer.empty
          resolvedNames.foreach { resolvedName =>
            val res = resolveQualifiedNameIn(resolvedName, rest.init, rest.last)
            res match {
              case Left(resolutionErr) => return Left(resolutionErr)
              case Right(resolved) =>
                allResolvedNames ++= resolved
            }
          }
          Right(allResolvedNames.toList)
        })
  }

  private def findExportedSymbolsFor(
    name: String
  ): List[ResolvedName] = {
    exportedSymbols.getOrElse(name, List())
  }

  /** Resolves a name exported by this module.
    *
    * @param name the name to resolve
    * @return the resolution for `name`
    */
  def resolveExportedName(
    name: String
  ): Either[ResolutionError, List[ResolvedName]] = {
    handleAmbiguity(findExportedSymbolsFor(name))
  }

  /** Dumps the export statements from this module into a structure ready for
    * further analysis. It uses only [[resolvedImports]] field, as
    *  [[exportedSymbols]] fields are expected to be filled later.
    *
    * For every symbol that is exported from this bindings map, gathers the module
    * in which the symbol is defined and returns it in the list. For example, if there
    * is an export `export project.Module.method`, there will be `Module` in the returned list.
    *
    * @return a list of triples of the exported module, the name it is exported
    *         as and any further symbol restrictions.
    */
  def getDirectlyExportedModules: List[ExportedModule] =
    resolvedImports
      .collect { case ResolvedImport(_, exports, targets) =>
        exports.flatMap { exp =>
          val exportAs = exp.rename match {
            case Some(rename) => Some(rename.name)
            case None         => None
          }
          val symbols = exp.onlyNames match {
            case Some(onlyNames) =>
              onlyNames.map(_.name)
            case None =>
              List(exp.name.parts.last.name)
          }
          targets.map {
            case m: ResolvedModule => ExportedModule(m, exportAs, symbols)
            case ResolvedType(modRef, _) =>
              ExportedModule(ResolvedModule(modRef), exportAs, symbols)
            case ResolvedConstructor(ResolvedType(modRef, _), _) =>
              ExportedModule(ResolvedModule(modRef), exportAs, symbols)
            case ResolvedModuleMethod(modRef, _) =>
              ExportedModule(ResolvedModule(modRef), exportAs, symbols)
            case ResolvedExtensionMethod(modRef, _) =>
              ExportedModule(ResolvedModule(modRef), exportAs, symbols)
            case ResolvedConversionMethod(modRef, _) =>
              ExportedModule(ResolvedModule(modRef), exportAs, symbols)
          }
        }
      }
      .flatten
      .distinct
}

object BindingsMap {

  private def handleAmbiguity(
    candidates: List[ResolvedName]
  ): Either[ResolutionError, List[ResolvedName]] = {
    candidates.distinct match {
      case List()   => Left(ResolutionNotFound)
      case List(it) => Right(List(it))
      case items =>
        val areAllResolvedMethods =
          items.forall(_.isInstanceOf[ResolvedMethod])
        if (areAllResolvedMethods) {
          items
            .map(_.asInstanceOf[ResolvedMethod])
            .groupBy(_.methodName)
            .values
            .toList match {
            case List(single) => Right(single)
            case _            => Left(ResolutionAmbiguous(items))
          }
        } else {
          Left(ResolutionAmbiguous(items))
        }
    }
  }

  /** A representation of a resolved export statement.
    *
    * @param module the target being exported.
    * @param exportedAs the name it is exported as.
    * @param symbols List of symbols connected to the export. The symbol refers to the last part
    *                of the physical name of the target being exported. It is not a fully qualified
    *                name.
    */
  case class ExportedModule(
    module: ResolvedModule,
    exportedAs: Option[String],
    symbols: List[String]
  ) {
    org.enso.common.Asserts.assertInJvm(
      symbols.forall(!_.contains(".")),
      "Not expected fully qualified names as symbols"
    )
    if (exportedAs.isDefined) {
      org.enso.common.Asserts.assertInJvm(
        !exportedAs.get.contains("."),
        "Not expected fully qualified name as `exportedAs`"
      )
    }

    /** Convert the internal [[ModuleReference]] to an abstract reference.
      *
      * @return `this` with its module reference made abstract
      */
    def toAbstract: ExportedModule = {
      this.copy(module = module.toAbstract)
    }

    /** Convert the internal [[ModuleReference]] to a concrete reference.
      *
      * @param moduleMap the mapping from qualified names to modules
      * @return `this` with its module reference made concrete
      */
    def toConcrete(moduleMap: ModuleMap): Option[ExportedModule] = {
      module.toConcrete(moduleMap).map { target =>
        this.copy(module = target)
      }
    }
  }

  sealed trait ImportTarget extends ResolvedName {
    override def toAbstract:                       ImportTarget
    override def toConcrete(moduleMap: ModuleMap): Option[ImportTarget]
    def findExportedSymbolsFor(name: String):      List[ResolvedName]

    /** Quicker conversion to library name */
    private[BindingsMap] lazy val toLibraryName: Option[LibraryName] = {
      val qnp = qualifiedName.path
      if (qnp.length >= 2) {
        Some(LibraryName(qnp(0), qnp(1)))
      } else {
        None
      }
    }

    /** Resolves the symbol with the given name in the context of this import target.
      * Note that it is valid to have multiple resolved names for a single symbol name,
      * for example, if the symbol is a name of an extension method and there are multiple
      * extension methods with the same name defined on multiple types.
      *
      * @param name (Unqualified) name of the symbol to resolve
      * @see [[BindingsMap.resolveName()]]
      */
    def resolveExportedSymbol(
      name: String
    ): Either[ResolutionError, List[ResolvedName]] =
      BindingsMap.handleAmbiguity(findExportedSymbolsFor(name))

    def exportedSymbols: Map[String, List[ResolvedName]]
  }

  /** A representation of a resolved import statement.
    *
    * @param importDef the definition of the import
    * @param exports the exports associated with the import
    * @param targets list of targets that this import resolves to. Note that it is valid for a single
    *                import to resolve to multiple entities, for example, in case of extension methods.
    */
  case class ResolvedImport(
    importDef: ir.module.scope.Import.Module,
    exports: List[ir.module.scope.Export.Module],
    targets: List[ImportTarget]
  ) {
    org.enso.common.Asserts.assertInJvm(targets.nonEmpty)
    org.enso.common.Asserts.assertInJvm(
      areTargetsConsistent(),
      "All targets must be either static methods or conversion methods"
    )

    /** Convert the internal [[ModuleReference]] to an abstract reference.
      *
      * @return `this` with its module reference made abstract
      */
    def toAbstract: ResolvedImport = {
      this.copy(targets = targets.map(_.toAbstract))
    }

    /** Convert the internal [[ModuleReference]] to a concrete reference.
      *
      * @param moduleMap the mapping from qualified names to modules
      * @return `this` with its module reference made concrete
      */
    def toConcrete(moduleMap: ModuleMap): Option[ResolvedImport] = {
      val newTargets = targets.map(_.toConcrete(moduleMap))
      if (newTargets.forall(_.isDefined)) {
        Some(this.copy(targets = newTargets.map(_.get)))
      } else {
        None
      }
    }

    /** Determines if this resolved import statement was generated by the compiler.
      *
      * @return true, if generated by the compiler, false otherwise
      */
    def isSynthetic(): Boolean = {
      importDef.isSynthetic
    }

    def findExportedSymbolsFor(name: String): List[ResolvedName] = {
      targets.flatMap(_.findExportedSymbolsFor(name))
    }

    private def areTargetsConsistent(): Boolean = {
      if (targets.size > 1) {
        // If there are multiple targets, they can either all be static methods, or all be
        // conversion methods.
        val allStaticMethods =
          targets.forall(_.isInstanceOf[ResolvedExtensionMethod])
        val allConversionMethods =
          targets.forall(_.isInstanceOf[ResolvedConversionMethod])
        allStaticMethods || allConversionMethods
      } else {
        true
      }
    }
  }

  sealed trait DefinedEntity {
    def name: String

    def resolvedIn(module: ModuleReference): ResolvedName = this match {
      case t: Type => ResolvedType(module, t)
      case staticMethod: ExtensionMethod =>
        ResolvedExtensionMethod(module, staticMethod)
      case conversionMethod: ConversionMethod =>
        ResolvedConversionMethod(module, conversionMethod)
      case m: ModuleMethod   => ResolvedModuleMethod(module, m)
      case p: PolyglotSymbol => ResolvedPolyglotSymbol(module, p)
    }

    def resolvedIn(module: Module): ResolvedName = resolvedIn(
      ModuleReference.Concrete(module)
    )

    // Determines if this entity can be exported during export resolution pass
    def canExport: Boolean
  }

  /** A representation of a constructor.
    *
    * @param name the name of the constructor.
    * @param arguments description of constructor's arguments
    * @param isProjectPrivate whether this constructor is project-private.
    */
  case class Cons(
    name: String,
    arguments: List[Argument],
    isProjectPrivate: Boolean
  ) {

    /** The number of fields in the constructor. */
    def arity: Int = arguments.length

    /** Whether all fields provide a default value. */
    def allFieldsDefaulted: Boolean = arguments.forall(_.hasDefaultValue)
  }

  case class Argument(name: String, hasDefaultValue: Boolean)

  /** A representation of a type with constructors.
    *
    * @param name the type name
    * @param members the member names
    * @param builtinType true if constructor is annotated with @Builtin_Type, false otherwise.
    * @param isPrivate if a type is considered private (for example because it is defined in a private module) -
    *        constructors and fields of a private type are not accessible outside the defining project
    */
  case class Type(
    override val name: String,
    params: Seq[String],
    members: Seq[Cons],
    builtinType: Boolean,
    isPrivate: Boolean
  ) extends DefinedEntity {
    override def canExport: Boolean = true
  }

  object Type {
    def fromIr(
      ir: Definition.Type,
      isBuiltinType: Boolean,
      isPrivate: Boolean
    ): Type =
      BindingsMap.Type(
        ir.name.name,
        ir.params.map(_.name.name),
        ir.members.map(m =>
          Cons(
            m.name.name,
            m.arguments.map { arg =>
              BindingsMap.Argument(
                arg.name.name,
                arg.defaultValue.isDefined
              )
            },
            m.isPrivate
          )
        ),
        isBuiltinType,
        isPrivate
      )
  }

  /** A representation of an imported polyglot symbol.
    *
    * @param name the name of the symbol.
    */
  case class PolyglotSymbol(override val name: String) extends DefinedEntity {
    override def canExport: Boolean = false
  }

  sealed trait Method extends DefinedEntity {
    override def canExport: Boolean = true
  }

  /** A representation of a method defined on the module, that is, a method
    * that is not defined on any type, but directly on a module.
    *
    * @param name the name of the method.
    */
  case class ModuleMethod(override val name: String) extends Method {}

  /** Static or extension method. Note that from the perspective of the runtime, there is no difference
    * between a static or an extension method. In the following snippet, both methods are considered
    * a duplicate:
    * ```
    * type My_Type
    *     method = 42
    * My_Type.method = 42
    * ```
    */
  case class ExtensionMethod(
    methodName: String,
    tpName: String
  ) extends Method {

    override def name: String = methodName
  }

  case class ConversionMethod(
    methodName: String,
    sourceTpName: String,
    targetTpName: String
  ) extends Method {
    override def name: String = methodName

    override def toString: String =
      targetTpName + ".from (other:" + sourceTpName + ")"
  }

  /** A name resolved to a sum type.
    *
    * @param module the module defining the type
    * @param tp a representation for the type
    */
  case class ResolvedType(override val module: ModuleReference, tp: Type)
      extends ResolvedName
      with ImportTarget {
    def getVariants: Seq[ResolvedConstructor] = {
      tp.members.map(ResolvedConstructor(this, _))
    }

    /** @inheritdoc */
    override def toAbstract: ResolvedType =
      this.copy(module = module.toAbstract)

    /** @inheritdoc */
    override def toConcrete(
      moduleMap: ModuleMap
    ): Option[ResolvedType] = for {
      concreteModule <- module.toConcrete(moduleMap)
    } yield ResolvedType(concreteModule, tp)

    override def qualifiedName: QualifiedName =
      module.getName.createChild(tp.name)

    override def findExportedSymbolsFor(name: String): List[ResolvedName] =
      exportedSymbols.getOrElse(name, List())

    override lazy val exportedSymbols: Map[String, List[ResolvedName]] =
      tp.members.map(m => (m.name, List(ResolvedConstructor(this, m)))).toMap
  }

  /** A result of successful name resolution.
    */
  sealed trait ResolvedName {

    def module: ModuleReference

    def qualifiedName: QualifiedName

    /** Convert the resolved name to abstract form.
      *
      * @return `this`, converted to abstract form
      */
    def toAbstract: ResolvedName

    /** Convert the resolved name to concrete form.
      *
      * @param moduleMap the mapping from qualified names to modules
      * @return `this`, converted to concrete form
      */
    def toConcrete(moduleMap: ModuleMap): Option[ResolvedName]
  }

  /** A representation of a name being resolved to a constructor.
    *
    * @param tpe the type the constructor is defined in.
    * @param cons a representation of the constructor.
    */
  case class ResolvedConstructor(tpe: ResolvedType, cons: Cons)
      extends ResolvedName
      with ImportTarget {

    /** @inheritdoc */
    override def toAbstract: ResolvedConstructor = {
      this.copy(tpe = tpe.toAbstract)
    }

    /** @inheritdoc */
    override def toConcrete(
      moduleMap: ModuleMap
    ): Option[ResolvedConstructor] = {
      tpe.toConcrete(moduleMap).map(tpe => this.copy(tpe = tpe))
    }

    /** @inheritdoc */
    override def qualifiedName: QualifiedName =
      tpe.qualifiedName.createChild(cons.name)

    /** @inheritdoc */
    override def module: ModuleReference = tpe.module

    override def findExportedSymbolsFor(name: String): List[ResolvedName] = {
      if (name == cons.name) {
        List(this)
      } else {
        List()
      }
    }

    override def exportedSymbols: Map[String, List[ResolvedName]] = {
      Map(cons.name -> List(this))
    }
  }

  /** A representation of a name being resolved to a module.
    *
    * @param module the module the name resolved to.
    */
  case class ResolvedModule(module: ModuleReference)
      extends ResolvedName
      with ImportTarget {

    /** @inheritdoc */
    override def toAbstract: ResolvedModule = {
      this.copy(module = module.toAbstract)
    }

    /** @inheritdoc */
    override def toConcrete(
      moduleMap: ModuleMap
    ): Option[ResolvedModule] = {
      module.toConcrete(moduleMap).map(module => this.copy(module = module))
    }

    override def qualifiedName: QualifiedName = module.getName

    override def findExportedSymbolsFor(name: String): List[ResolvedName] =
      exportedSymbols.getOrElse(name, List())

    override def exportedSymbols: Map[String, List[ResolvedName]] =
      module
        .unsafeAsModule("must be a module to run resolution")
        .getIr
        .unsafeGetMetadata(
          BindingAnalysis,
          "Wrong pass ordering. Running resolution on an unparsed module."
        )
        .exportedSymbols
  }

  sealed trait ResolvedMethod extends ResolvedName with ImportTarget {
    def methodName: String

    override def findExportedSymbolsFor(name: String): List[ResolvedName] = {
      if (name == methodName) {
        List(this)
      } else {
        List()
      }
    }

    override def exportedSymbols: Map[String, List[ResolvedName]] = Map(
      methodName -> List(this)
    )

    override def qualifiedName: QualifiedName = {
      module.getName.createChild(methodName)
    }
  }

  /** A representation of a resolved method defined directly on module.
    *
    * @param module the module defining the method.
    * @param method the method representation.
    */
  case class ResolvedModuleMethod(module: ModuleReference, method: ModuleMethod)
      extends ResolvedMethod {

    /** @inheritdoc */
    override def toAbstract: ResolvedModuleMethod = {
      this.copy(module = module.toAbstract)
    }

    /** @inheritdoc */
    override def toConcrete(
      moduleMap: ModuleMap
    ): Option[ResolvedModuleMethod] = {
      module.toConcrete(moduleMap).map(module => this.copy(module = module))
    }

    def getIr: Option[ir.module.scope.Definition] = {
      val moduleIr = module match {
        case ModuleReference.Concrete(module) => Some(module.getIr)
        case ModuleReference.Abstract(_)      => None
      }
      moduleIr.flatMap(_.bindings.find {
        case method: ir.module.scope.definition.Method.Explicit =>
          method.methodReference.methodName.name == this.method.name && method.methodReference.typePointer
            .forall(
              _.getMetadata(MethodDefinitions.INSTANCE)
                .contains(Resolution(ResolvedModule(module)))
            )
        case _ => false
      })
    }

    def unsafeGetIr(missingMessage: String): ir.module.scope.Definition =
      getIr.getOrElse(throw new CompilerError(missingMessage))

    override def methodName: String = method.name
  }

  /** Method resolved on a type - either static method or extension method.
    */
  case class ResolvedExtensionMethod(
    module: ModuleReference,
    staticMethod: ExtensionMethod
  ) extends ResolvedMethod {
    override def toAbstract: ResolvedExtensionMethod = {
      this.copy(module = module.toAbstract)
    }

    override def toConcrete(
      moduleMap: ModuleMap
    ): Option[ResolvedExtensionMethod] = {
      module.toConcrete(moduleMap).map { module =>
        this.copy(module = module)
      }
    }

    override def methodName: String = staticMethod.methodName
  }

  case class ResolvedConversionMethod(
    module: ModuleReference,
    conversionMethod: ConversionMethod
  ) extends ResolvedMethod {
    override def toAbstract: ResolvedConversionMethod = {
      this.copy(module = module.toAbstract)
    }

    override def toConcrete(
      moduleMap: ModuleMap
    ): Option[ResolvedConversionMethod] = {
      module.toConcrete(moduleMap).map { module =>
        this.copy(module = module)
      }
    }

    override def methodName: String = conversionMethod.methodName
  }

  /** A representation of a name being resolved to a polyglot symbol.
    *
    * @param symbol the imported symbol name.
    */
  case class ResolvedPolyglotSymbol(
    module: ModuleReference,
    symbol: PolyglotSymbol
  ) extends ResolvedName {

    /** @inheritdoc */
    override def toAbstract: ResolvedPolyglotSymbol = {
      this.copy(module = module.toAbstract)
    }

    /** @inheritdoc */
    override def toConcrete(
      moduleMap: ModuleMap
    ): Option[ResolvedPolyglotSymbol] = {
      module.toConcrete(moduleMap).map(module => this.copy(module = module))
    }

    override def qualifiedName: QualifiedName =
      module.getName.createChild(symbol.name)

    def findExportedSymbolFor(
      name: String
    ): org.enso.compiler.data.BindingsMap.ResolvedName =
      ResolvedPolyglotField(this, name)
  }

  case class ResolvedPolyglotField(symbol: ResolvedPolyglotSymbol, name: String)
      extends ResolvedName {
    def module: BindingsMap.ModuleReference = symbol.module
    def qualifiedName: QualifiedName        = symbol.qualifiedName.createChild(name)
    def toAbstract: ResolvedName =
      ResolvedPolyglotField(symbol.toAbstract, name)
    def toConcrete(moduleMap: ModuleMap): Option[ResolvedName] =
      symbol.toConcrete(moduleMap).map(ResolvedPolyglotField(_, name))
  }

  /** A representation of an error during name resolution.
    */
  sealed trait ResolutionError extends errors.Resolution.ExplainResolution

  /** A representation of a resolution error due to symbol ambiguity.
    *
    * @param candidates all the possible resolutions for the name.
    */
  case class ResolutionAmbiguous(candidates: List[ResolvedName])
      extends ResolutionError {
    override def explain(originalName: ir.Name): String = {
      val firstLine =
        s"The name ${originalName.name} is ambiguous. Possible candidates are:"
      val lines = candidates.map {
        case BindingsMap.ResolvedConstructor(
              definitionType,
              cons
            ) =>
          s"    Constructor ${cons.name} defined in module ${definitionType.module.getName};"
        case BindingsMap.ResolvedModule(module) =>
          s"    The module ${module.getName};"
        case BindingsMap.ResolvedPolyglotSymbol(_, symbol) =>
          s"    The imported polyglot symbol ${symbol.name};"
        case BindingsMap.ResolvedPolyglotField(_, name) =>
          s"    The imported polyglot field ${name};"
        case BindingsMap.ResolvedModuleMethod(module, symbol) =>
          s"    The method ${symbol.name} defined in module ${module.getName}"
        case BindingsMap.ResolvedExtensionMethod(module, staticMethod) =>
          s"    The static method ${staticMethod.methodName} defined in module ${module.getName} for type ${staticMethod.tpName}"
        case BindingsMap.ResolvedConversionMethod(module, conversionMethod) =>
          s"    The conversion method ${conversionMethod.targetTpName}.${conversionMethod.methodName} defined in module ${module.getName}"
        case BindingsMap.ResolvedType(module, typ) =>
          s"    Type ${typ.name} defined in module ${module.getName}"
      }
      (firstLine :: lines).mkString("\n")
    }
  }

  /** A resolution error due to the symbol not being found.
    */
  case object ResolutionNotFound extends ResolutionError {
    override def explain(originalName: ir.Name): String =
      s"The name `${originalName.name}` could not be found"
  }

  /** A resolution error due to usage of Self type reference outside of a type scope.
    */
  case object SelfTypeOutsideOfTypeDefinition extends ResolutionError {
    override def explain(originalName: ir.Name): String =
      s"The Self type is not applicable outside of a type definition"
  }

  /** A metadata-friendly storage for resolutions */
  case class Resolution(target: ResolvedName) extends IRPass.IRMetadata {

    /** The name of the metadata as a string. */
    override val metadataName: String = "Resolution"

    /** @inheritdoc */
    override def prepareForSerialization(compiler: Compiler): Resolution =
      this.copy(target = this.target.toAbstract)

    /** @inheritdoc */
    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[Resolution] = {
      val moduleMap = compiler.getPackageRepository.getModuleMap
      this.target.toConcrete(moduleMap).map(t => this.copy(target = t))
    }

    /** Creates a duplicate of this metadata if applicable.
      *
      * This method should employ deep-copy semantics where appropriate. It may
      * return None to indicate that this metadata should not be preserved
      * during duplication.
      *
      * @return Some duplicate of this metadata or None if this metadata should
      *         not be preserved
      */
    override def duplicate(): Option[IRPass.IRMetadata] = Some(this)
  }

  /** A reference to a module.
    */
  sealed trait ModuleReference {

    /** @return the qualified name of the module
      */
    def getName: QualifiedName

    /** Convert `this` into a concrete module reference.
      *
      * @param moduleMap the mapping from qualified names to concrete modules
      * @return the concrete module for this reference, if possible
      */
    def toConcrete(
      moduleMap: PackageRepository.ModuleMap
    ): Option[ModuleReference.Concrete]

    /** Convert `this` into an abstract module reference.
      *
      * @return the abstract reference to the module represented by `this`
      */
    def toAbstract: ModuleReference.Abstract

    /** Unsafely coerces the module reference to a concrete one.
      *
      * @param message the message for if the coercion fails
      * @return the concrete version of this reference
      */
    @throws[CompilerError]
    def unsafeAsModule(message: String = ""): Module
  }
  object ModuleReference {

    /** A module reference that points to a concrete [[Module]] object.
      *
      * @param module the module being referenced
      */
    case class Concrete(module: Module) extends ModuleReference {

      /** @inheritdoc */
      override def getName: QualifiedName = module.getName

      /** Converts `this` into a concrete module reference (a no-op).
        *
        * @param moduleMap the mapping from qualified names to concrete modules
        * @return the concrete module for this reference, if possible
        */
      override def toConcrete(moduleMap: ModuleMap): Option[Concrete] =
        Some(this)

      /** @inheritdoc */
      override lazy val toAbstract: Abstract =
        ModuleReference.Abstract(module.getName)

      /** @inheritdoc */
      override def unsafeAsModule(message: String = ""): Module = module
    }

    /** A module reference that refers to a module by qualified name, without an
      * explicit link to the target.
      *
      * @param name the qualified name (including namespace) of the module
      *             being referenced
      */
    case class Abstract(name: QualifiedName) extends ModuleReference {

      /** @inheritdoc */
      override def getName: QualifiedName = name

      /** @inheritdoc */
      override def toConcrete(moduleMap: ModuleMap): Option[Concrete] = {
        moduleMap.get(name.toString).map(Concrete)
      }

      /** Convert `this` into an abstract module reference (a no-op).
        *
        * @return the abstract reference to the module represented by `this`
        */
      override def toAbstract: Abstract = this

      /** @inheritdoc */
      override def unsafeAsModule(message: String = ""): Module = {
        val rest = if (message.isEmpty) "." else s": $message"
        val errMsg =
          s"Could not get concrete module from abstract module $name$rest"

        throw new CompilerError(errMsg)
      }
    }
  }
}
