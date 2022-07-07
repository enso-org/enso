package org.enso.compiler.data

import org.enso.compiler.{Compiler, PackageRepository}
import org.enso.compiler.PackageRepository.ModuleMap
import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap.ModuleReference
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.compiler.pass.resolve.MethodDefinitions
import org.enso.interpreter.runtime.Module
import org.enso.pkg.QualifiedName

import java.io.ObjectOutputStream
import scala.annotation.unused

/** A utility structure for resolving symbols in a given module.
  *
  * @param constructors the types defined in the current module
  * @param polyglotSymbols the polyglot symbols imported into the scope
  * @param moduleMethods the methods defined with current module as `this`
  * @param currentModule the module holding these bindings
  */
case class BindingsMap(
  types: List[BindingsMap.Type],
  constructors: List[BindingsMap.Cons],
  polyglotSymbols: List[BindingsMap.PolyglotSymbol],
  moduleMethods: List[BindingsMap.ModuleMethod],
  currentModule: ModuleReference
) extends IRPass.Metadata {
  import BindingsMap._

  override val metadataName: String = "Bindings Map"

  override def duplicate(): Option[IRPass.Metadata] = Some(this)

  /** Other modules, imported by [[currentModule]].
    */
  var resolvedImports: List[ResolvedImport] = List()

  /** Modules exported by [[currentModule]].
    */
  var resolvedExports: List[ExportedModule] = List()

  /** Symbols exported by [[currentModule]].
    */
  var exportedSymbols: Map[String, List[ResolvedName]] = Map()

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
    val packageRepository = compiler.context.getPackageRepository
    this.toConcrete(packageRepository.getModuleMap)
  }

  /** Convert this [[BindingsMap]] instance to use abstract module references.
    *
    * @return `this` with module references converted to abstract
    */
  def toAbstract: BindingsMap = {
    val copy = this.copy(currentModule = currentModule.toAbstract)
    copy.resolvedImports = this.resolvedImports.map(_.toAbstract)
    copy.resolvedExports = this.resolvedExports.map(_.toAbstract)
    copy.exportedSymbols = this.exportedSymbols.map { case (key, value) =>
      key -> value.map(name => name.toAbstract)
    }

    copy
  }

  /** Convert this [[BindingsMap]] instance to use concrete module references.
    *
    * @param moduleMap the mapping from qualified module names to module
    *                  instances
    * @return `this` with module references converted to concrete
    */
  def toConcrete(moduleMap: ModuleMap): Option[BindingsMap] = {
    val newMap = this.currentModule.toConcrete(moduleMap).map { module =>
      this.copy(currentModule = module)
    }

    val withImports: Option[BindingsMap] = newMap.flatMap { bindings =>
      val newImports = this.resolvedImports.map(_.toConcrete(moduleMap))
      if (newImports.exists(_.isEmpty)) {
        None
      } else {
        bindings.resolvedImports = newImports.map(_.get)
        Some(bindings)
      }
    }

    val withExports: Option[BindingsMap] = withImports.flatMap { bindings =>
      val newExports = this.resolvedExports.map(_.toConcrete(moduleMap))
      if (newExports.exists(_.isEmpty)) {
        None
      } else {
        bindings.resolvedExports = newExports.map(_.get)
        Some(bindings)
      }
    }

    val withSymbols: Option[BindingsMap] = withExports.flatMap { bindings =>
      val newSymbols = this.exportedSymbols.map { case (key, value) =>
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
        bindings.exportedSymbols = newSymbols.map { case (k, v) => k -> v.get }
        Some(bindings)
      }
    }

    withSymbols
  }

  private def findConstructorCandidates(
    name: String
  ): List[ResolvedConstructor] = {
    constructors
      .filter(_.name == name)
      .map(ResolvedConstructor(currentModule, _))
  }

  private def findPolyglotCandidates(
    name: String
  ): List[ResolvedPolyglotSymbol] = {
    polyglotSymbols
      .filter(_.name == name)
      .map(ResolvedPolyglotSymbol(currentModule, _))
  }

  private def findMethodCandidates(
    name: String
  ): List[ResolvedName] =
    moduleMethods.filter(_.name == name).map(ResolvedMethod(currentModule, _))

  private def findLocalCandidates(name: String): List[ResolvedName] = {
    val conses   = findConstructorCandidates(name)
    val polyglot = findPolyglotCandidates(name)
    val methods  = findMethodCandidates(name)
    val all      = conses ++ polyglot ++ methods
    if (all.isEmpty && currentModule.getName.item == name) {
      List(ResolvedModule(currentModule))
    } else { all }
  }

  private def findQualifiedImportCandidates(
    name: String
  ): List[ResolvedName] = {
    resolvedImports
      .filter(_.importDef.getSimpleName.name == name)
      .map(res => ResolvedModule(res.module))
  }

  private def findExportedCandidatesInImports(
    name: String
  ): List[ResolvedName] = {

    resolvedImports
      .flatMap { imp =>
        if (imp.importDef.allowsAccess(name)) {
          imp.module match {
            case ModuleReference.Concrete(module) =>
              module.getIr
                .unsafeGetMetadata(
                  BindingAnalysis,
                  "Wrong pass ordering. Running resolution on an unparsed module."
                )
                .findExportedSymbolsFor(name)
            case ModuleReference.Abstract(name) =>
              throw new CompilerError(
                s"Cannot find export candidates for abstract module reference $name."
              )
          }
        } else { List() }
      }
  }

  private def handleAmbiguity(
    candidates: List[ResolvedName]
  ): Either[ResolutionError, ResolvedName] = {
    candidates.distinct match {
      case List()   => Left(ResolutionNotFound)
      case List(it) => Right(it)
      case items    => Left(ResolutionAmbiguous(items))
    }
  }

  /** Resolves a name as a type.
    *
    * NB: This should be removed when sum types become proper runtime values.
    *
    * @param name the type name to resolve.
    * @return the resolution
    */
  def resolveTypeName(
    name: String
  ): Either[ResolutionError, ResolvedTypeName] = {
    types.find(_.name == name) match {
      case Some(value) => Right(ResolvedType(currentModule, value))
      case None        => resolveName(name)
    }
  }

  /** Resolves a name in the context of current module.
    *
    * @param name the name to resolve.
    * @return a resolution for `name` or an error, if the name could not be
    *         resolved.
    */
  def resolveName(
    name: String
  ): Either[ResolutionError, ResolvedName] = {
    val local = findLocalCandidates(name)
    if (local.nonEmpty) {
      return handleAmbiguity(local)
    }
    val qualifiedImps = findQualifiedImportCandidates(name)
    if (qualifiedImps.nonEmpty) {
      return handleAmbiguity(qualifiedImps)
    }
    handleAmbiguity(findExportedCandidatesInImports(name))
  }

  /** Resolves a qualified name to a symbol in the context of this module.
    *
    * @param name the name to resolve
    * @return a resolution for `name`
    */

  def resolveQualifiedName(
    name: List[String]
  ): Either[ResolutionError, ResolvedName] =
    name match {
      case List()     => Left(ResolutionNotFound)
      case List(item) => resolveName(item)
      case firstModuleName :: rest =>
        val consName = rest.last
        val modNames = rest.init
        resolveName(firstModuleName).flatMap {
          case ResolvedModule(mod) =>
            val firstModBindings: BindingsMap = getBindingsFrom(mod)
            var currentModule                 = firstModBindings
            for (modName <- modNames) {
              val resolution = currentModule.resolveExportedName(modName)
              resolution match {
                case Left(err) => return Left(err)
                case Right(ResolvedModule(mod)) =>
                  currentModule = getBindingsFrom(mod)
                case _ => return Left(ResolutionNotFound)
              }
            }
            currentModule.resolveExportedName(consName)
          case _ => Left(ResolutionNotFound)
        }
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
  ): Either[ResolutionError, ResolvedName] = {
    handleAmbiguity(findExportedSymbolsFor(name))
  }

  /** Dumps the export statements from this module into a structure ready for
    * further analysis.
    *
    * @return a list of triples of the exported module, the name it is exported
    *         as and any further symbol restrictions.
    */
  def getDirectlyExportedModules: List[ExportedModule] =
    resolvedImports.collect { case ResolvedImport(_, Some(exp), mod) =>
      val hidingEnsoProject =
        SymbolRestriction.Hiding(Set(Generated.ensoProjectMethodName))
      val restriction = if (exp.isAll) {
        val definedRestriction = if (exp.onlyNames.isDefined) {
          SymbolRestriction.Only(
            exp.onlyNames.get
              .map(name =>
                SymbolRestriction
                  .AllowedResolution(name.name.toLowerCase, None)
              )
              .toSet
          )
        } else if (exp.hiddenNames.isDefined) {
          SymbolRestriction.Hiding(
            exp.hiddenNames.get.map(_.name.toLowerCase).toSet
          )
        } else {
          SymbolRestriction.All
        }
        SymbolRestriction.Intersect(
          List(hidingEnsoProject, definedRestriction)
        )
      } else {
        SymbolRestriction.Only(
          Set(
            SymbolRestriction.AllowedResolution(
              exp.getSimpleName.name.toLowerCase,
              Some(ResolvedModule(mod))
            )
          )
        )
      }
      val rename = if (!exp.isAll) {
        Some(exp.getSimpleName.name)
      } else {
        None
      }
      ExportedModule(mod, rename, restriction)
    }
}

object BindingsMap {

  private def getBindingsFrom(module: ModuleReference): BindingsMap = {
    module match {
      case ModuleReference.Concrete(module) =>
        module.getIr.unsafeGetMetadata(
          BindingAnalysis,
          "imported module has no binding map info"
        )
      case ModuleReference.Abstract(_) =>
        throw new CompilerError(
          "Bindings cannot be obtained from an abstract module reference."
        )
    }
  }

  /** Utilities for methods automatically generated by the compiler.
    */
  object Generated {

    /** The name of the builtin `enso_project` method.
      */
    val ensoProjectMethodName: String = "enso_project"
  }

  /** Represents a symbol restriction on symbols exported from a module. */
  sealed trait SymbolRestriction {

    /** Whether the export statement allows accessing the given name.
      *
      * @param symbol the name to check
      * @param resolution the particular resolution of `symbol`
      * @return whether access to the symbol is permitted by this restriction.
      */
    def canAccess(symbol: String, resolution: ResolvedName): Boolean

    /** Performs static optimizations on the restriction, simplifying
      * common patterns.
      *
      * @return a possibly simpler version of the restriction, describing
      *         the same set of names.
      */
    def optimize: SymbolRestriction

    /** Convert any internal [[ModuleReference]]s to abstract references.
      *
      * @return `this` with any module references made abstract
      */
    def toAbstract: SymbolRestriction

    /** Convert any internal [[ModuleReference]]s to concrete references.
      *
      * @param moduleMap the mapping from qualified names to modules
      * @return `this` with its module reference made concrete
      */
    def toConcrete(moduleMap: ModuleMap): Option[SymbolRestriction]
  }

  object SymbolRestriction {

    /** A representation of allowed symbol. An allowed symbol consists of
      * a name and an optional resolution refinement.
      *
      * @param symbol the symbol name
      * @param resolution the only allowed resolution of `symbol`
      */
    case class AllowedResolution(
      symbol: String,
      resolution: Option[ResolvedName]
    ) {

      /** Checks if the `symbol` is visible under this restriction, with
        * a given resolution.
        *
        * @param symbol the symbol
        * @param resolution `symbol`'s resolution
        * @return `true` if the symbol is visible, `false` otherwise
        */
      def allows(symbol: String, resolution: ResolvedName): Boolean = {
        val symbolMatch = this.symbol == symbol.toLowerCase
        val resolutionMatch =
          this.resolution.isEmpty || this.resolution.get == resolution
        symbolMatch && resolutionMatch
      }

      /** Convert the internal resolution to abstract form.
        *
        * @return `this` with its resolution converted to abstract form
        */
      def toAbstract: AllowedResolution = {
        this.copy(resolution = resolution.map(_.toAbstract))
      }

      /** Convert the internal resolution to concrete form.
        *
        * @param moduleMap the mapping from qualified names to modules
        * @return `this` with its resolution made concrete
        */
      def toConcrete(moduleMap: ModuleMap): Option[AllowedResolution] = {
        resolution match {
          case None => Some(this)
          case Some(res) =>
            res.toConcrete(moduleMap).map(r => this.copy(resolution = Some(r)))
        }
      }
    }

    /** A restriction representing a set of allowed symbols.
      *
      * @param symbols the allowed symbols.
      */
    case class Only(symbols: Set[AllowedResolution]) extends SymbolRestriction {

      /** @inheritdoc */
      override def canAccess(
        symbol: String,
        resolution: ResolvedName
      ): Boolean = symbols.exists(_.allows(symbol, resolution))

      /** @inheritdoc */
      override def optimize: SymbolRestriction = this

      /** @inheritdoc */
      override def toAbstract: Only = {
        this.copy(symbols = symbols.map(_.toAbstract))
      }

      /** @inheritdoc */
      //noinspection DuplicatedCode
      override def toConcrete(moduleMap: ModuleMap): Option[Only] = {
        val newSymbols = symbols.map(_.toConcrete(moduleMap))
        if (!newSymbols.exists(_.isEmpty)) {
          Some(this.copy(symbols = newSymbols.map(_.get)))
        } else None
      }
    }

    /** A restriction representing a set of excluded symbols.
      *
      * @param symbols the excluded symbols.
      */
    case class Hiding(symbols: Set[String]) extends SymbolRestriction {

      /** @inheritdoc */
      override def canAccess(
        symbol: String,
        resolution: ResolvedName
      ): Boolean = !symbols.contains(symbol.toLowerCase)

      /** @inheritdoc */
      override def optimize: Hiding = this

      /** @inheritdoc */
      override def toAbstract: Hiding = this

      /** @inheritdoc */
      override def toConcrete(moduleMap: ModuleMap): Option[Hiding] = Some(this)
    }

    /** A restriction meaning there's no restriction at all.
      */
    case object All extends SymbolRestriction {

      /** @inheritdoc */
      override def canAccess(
        symbol: String,
        resolution: ResolvedName
      ): Boolean = true

      /** @inheritdoc */
      override def optimize: SymbolRestriction = this

      /** @inheritdoc */
      override def toAbstract: All.type = this

      /** @inheritdoc */
      override def toConcrete(moduleMap: ModuleMap): Option[All.type] = Some(
        this
      )
    }

    /** A complete restriction – no symbols are permitted
      */
    case object Empty extends SymbolRestriction {

      /** @inheritdoc */
      override def canAccess(
        symbol: String,
        resolution: ResolvedName
      ): Boolean = false

      /** @inheritdoc */
      override def optimize: SymbolRestriction = this

      /** @inheritdoc */
      override def toAbstract: Empty.type = this

      /** @inheritdoc */
      override def toConcrete(moduleMap: ModuleMap): Option[Empty.type] = Some(
        this
      )
    }

    /** An intersection of restrictions – a symbol is allowed if all components
      * allow it.
      *
      * @param restrictions the intersected restrictions.
      */
    case class Intersect(restrictions: List[SymbolRestriction])
        extends SymbolRestriction {

      /** @inheritdoc */
      override def canAccess(
        symbol: String,
        resolution: ResolvedName
      ): Boolean = restrictions.forall(_.canAccess(symbol, resolution))

      /** @inheritdoc */
      //noinspection DuplicatedCode
      override def optimize: SymbolRestriction = {
        val optimizedTerms = restrictions.map(_.optimize)
        val (intersects, otherTerms) =
          optimizedTerms.partition(_.isInstanceOf[Intersect])
        val allTerms = intersects.flatMap(
          _.asInstanceOf[Intersect].restrictions
        ) ++ otherTerms
        if (allTerms.contains(Empty)) {
          return Empty
        }
        val unions = allTerms.filter(_.isInstanceOf[Union])
        val onlys  = allTerms.collect { case only: Only => only }
        val hides  = allTerms.collect { case hiding: Hiding => hiding }
        val combinedOnlys = onlys match {
          case List() => None
          case items =>
            Some(Only(items.map(_.symbols).reduce(_.intersect(_))))
        }
        val combinedHiding = hides match {
          case List() => None
          case items =>
            Some(Hiding(items.map(_.symbols).reduce(_.union(_))))
        }
        val newTerms = combinedHiding.toList ++ combinedOnlys.toList ++ unions
        newTerms match {
          case List()   => All
          case List(it) => it
          case items    => Intersect(items)
        }
      }

      /** @inheritdoc */
      override def toAbstract: Intersect = {
        this.copy(restrictions = restrictions.map(_.toAbstract))
      }

      /** @inheritdoc */
      //noinspection DuplicatedCode
      override def toConcrete(moduleMap: ModuleMap): Option[Intersect] = {
        val newRestrictions = restrictions.map(_.toConcrete(moduleMap))
        if (!newRestrictions.exists(_.isEmpty)) {
          Some(this.copy(restrictions = newRestrictions.map(_.get)))
        } else None
      }
    }

    /** A union of restrictions – a symbol is allowed if any component allows
      * it.
      *
      * @param restrictions the component restricitons.
      */
    case class Union(restrictions: List[SymbolRestriction])
        extends SymbolRestriction {

      /** @inheritdoc */
      override def canAccess(
        symbol: String,
        resolution: ResolvedName
      ): Boolean = restrictions.exists(_.canAccess(symbol, resolution))

      /** @inheritdoc */
      //noinspection DuplicatedCode
      override def optimize: SymbolRestriction = {
        val optimizedTerms = restrictions.map(_.optimize)
        val (unions, otherTerms) =
          optimizedTerms.partition(_.isInstanceOf[Union])
        val allTerms = unions.flatMap(
          _.asInstanceOf[Union].restrictions
        ) ++ otherTerms
        if (allTerms.contains(All)) {
          return All
        }
        val intersects = allTerms.filter(_.isInstanceOf[Intersect])
        val onlys      = allTerms.collect { case only: Only => only }
        val hides      = allTerms.collect { case hiding: Hiding => hiding }
        val combinedOnlys = onlys match {
          case List() => None
          case items =>
            Some(Only(items.map(_.symbols).reduce(_.union(_))))
        }
        val combinedHiding = hides match {
          case List() => None
          case items =>
            Some(Hiding(items.map(_.symbols).reduce(_.intersect(_))))
        }
        val newTerms =
          combinedHiding.toList ++ combinedOnlys.toList ++ intersects
        newTerms match {
          case List()   => Empty
          case List(it) => it
          case items    => Union(items)
        }
      }

      /** @inheritdoc */
      override def toAbstract: Union = {
        this.copy(restrictions = restrictions.map(_.toAbstract))
      }

      /** @inheritdoc */
      //noinspection DuplicatedCode
      override def toConcrete(moduleMap: ModuleMap): Option[Union] = {
        val newRestrictions = restrictions.map(_.toConcrete(moduleMap))
        if (!newRestrictions.exists(_.isEmpty)) {
          Some(this.copy(restrictions = newRestrictions.map(_.get)))
        } else None
      }
    }
  }

  /** A representation of a resolved export statement.
    *
    * @param module the module being exported.
    * @param exportedAs the name it is exported as.
    * @param symbols any symbol restrictions connected to the export.
    */
  case class ExportedModule(
    module: ModuleReference,
    exportedAs: Option[String],
    symbols: SymbolRestriction
  ) {

    /** Convert the internal [[ModuleReference]] to an abstract reference.
      *
      * @return `this` with its module reference made abstract
      */
    def toAbstract: ExportedModule = {
      this.copy(module = module.toAbstract, symbols = symbols.toAbstract)
    }

    /** Convert the internal [[ModuleReference]] to a concrete reference.
      *
      * @param moduleMap the mapping from qualified names to modules
      * @return `this` with its module reference made concrete
      */
    def toConcrete(moduleMap: ModuleMap): Option[ExportedModule] = {
      module.toConcrete(moduleMap).flatMap { x =>
        symbols
          .toConcrete(moduleMap)
          .map(y => this.copy(module = x, symbols = y))
      }
    }
  }

  /** A representation of a resolved import statement.
    *
    * @param importDef the definition of the import
    * @param exports the exports associated with the import
    * @param module the module this import resolves to
    */
  case class ResolvedImport(
    importDef: IR.Module.Scope.Import.Module,
    exports: Option[IR.Module.Scope.Export.Module],
    module: ModuleReference
  ) {

    /** Convert the internal [[ModuleReference]] to an abstract reference.
      *
      * @return `this` with its module reference made abstract
      */
    def toAbstract: ResolvedImport = {
      this.copy(module = module.toAbstract)
    }

    /** Convert the internal [[ModuleReference]] to a concrete reference.
      *
      * @param moduleMap the mapping from qualified names to modules
      * @return `this` with its module reference made concrete
      */
    def toConcrete(moduleMap: ModuleMap): Option[ResolvedImport] = {
      module.toConcrete(moduleMap).map(x => this.copy(module = x))
    }
  }

  /** A representation of a constructor.
    *
    * @param name the name of the constructor.
    * @param arity the number of fields in the constructor.
    * @param allFieldsDefaulted whether all fields provide a default value.
    * @param builtinType true if constructor is annotated with @Builtin_Type, false otherwise.
    */
  case class Cons(
    name: String,
    arity: Int,
    allFieldsDefaulted: Boolean,
    builtinType: Boolean = false
  )

  /** A representation of a sum type
    *
    * @param name the type name
    * @param members the member names
    */
  case class Type(name: String, members: Seq[String])

  /** A representation of an imported polyglot symbol.
    *
    * @param name the name of the symbol.
    */
  case class PolyglotSymbol(name: String)

  /** A representation of a method defined on the current module.
    *
    * @param name the name of the method.
    */
  case class ModuleMethod(name: String)

  /** Represents a resolved name on typelevel.
    *
    * NB: should be unified with `ResolvedName` and removed, once sum types get
    * a proper runtime meaning.
    */
  sealed trait ResolvedTypeName {
    def module: ModuleReference

    /** Convert the resolved name to abstract form.
      *
      * @return `this`, converted to abstract form
      */
    def toAbstract: ResolvedTypeName

    /** Convert the resolved name to concrete form.
      *
      * @param moduleMap the mapping from qualified names to modules
      * @return `this`, converted to concrete form
      */
    def toConcrete(moduleMap: ModuleMap): Option[ResolvedTypeName]
  }

  /** A name resolved to a sum type.
    *
    * @param module the module defining the type
    * @param tp a representation for the type
    */
  case class ResolvedType(override val module: ModuleReference, tp: Type)
      extends ResolvedTypeName {
    def getVariants: Seq[ResolvedConstructor] = {
      val bindingsMap = getBindingsFrom(module)
      tp.members.flatMap(m =>
        bindingsMap.constructors
          .find(_.name == m)
          .map(ResolvedConstructor(module, _))
      )
    }

    /** @inheritdoc */
    override def toAbstract: ResolvedType = {
      this.copy(module = module.toAbstract)
    }

    /** @inheritdoc */
    override def toConcrete(
      moduleMap: ModuleMap
    ): Option[ResolvedType] = {
      module.toConcrete(moduleMap).map(module => this.copy(module = module))
    }

    def qualifiedName: QualifiedName = module.getName.createChild(tp.name)
  }

  /** A result of successful name resolution.
    */
  sealed trait ResolvedName extends ResolvedTypeName {

    /** Convert the resolved name to abstract form.
      *
      * @return `this`, converted to abstract form
      */
    override def toAbstract: ResolvedName

    /** Convert the resolved name to concrete form.
      *
      * @param moduleMap the mapping from qualified names to modules
      * @return `this`, converted to concrete form
      */
    override def toConcrete(moduleMap: ModuleMap): Option[ResolvedName]
  }

  /** A representation of a name being resolved to a constructor.
    *
    * @param module the module the constructor is defined in.
    * @param cons a representation of the constructor.
    */
  case class ResolvedConstructor(module: ModuleReference, cons: Cons)
      extends ResolvedName {

    /** @inheritdoc */
    override def toAbstract: ResolvedConstructor = {
      this.copy(module = module.toAbstract)
    }

    /** @inheritdoc */
    override def toConcrete(
      moduleMap: ModuleMap
    ): Option[ResolvedConstructor] = {
      module.toConcrete(moduleMap).map(module => this.copy(module = module))
    }

    def qualifiedName: QualifiedName = module.getName.createChild(cons.name)
  }

  /** A representation of a name being resolved to a module.
    *
    * @param module the module the name resolved to.
    */
  case class ResolvedModule(module: ModuleReference) extends ResolvedName {

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
  }

  /** A representation of a name being resolved to a method call.
    *
    * @param module the module defining the method.
    * @param method the method representation.
    */
  case class ResolvedMethod(module: ModuleReference, method: ModuleMethod)
      extends ResolvedName {

    /** @inheritdoc */
    override def toAbstract: ResolvedMethod = {
      this.copy(module = module.toAbstract)
    }

    /** @inheritdoc */
    override def toConcrete(
      moduleMap: ModuleMap
    ): Option[ResolvedMethod] = {
      module.toConcrete(moduleMap).map(module => this.copy(module = module))
    }

    def getIr: Option[IR.Module.Scope.Definition] = {
      val moduleIr = module match {
        case ModuleReference.Concrete(module) => Some(module.getIr)
        case ModuleReference.Abstract(_)      => None
      }
      moduleIr.flatMap(_.bindings.find {
        case method: IR.Module.Scope.Definition.Method.Explicit =>
          method.methodReference.methodName.name == this.method.name && method.methodReference.typePointer
            .map(
              _.getMetadata(MethodDefinitions)
                .contains(Resolution(ResolvedModule(module)))
            )
            .getOrElse(true)
        case _ => false
      })
    }

    def unsafeGetIr(missingMessage: String): IR.Module.Scope.Definition =
      getIr.getOrElse(throw new CompilerError(missingMessage))
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
  }

  /** A representation of an error during name resolution.
    */
  sealed trait ResolutionError

  /** A representation of a resolution error due to symbol ambiguity.
    *
    * @param candidates all the possible resolutions for the name.
    */
  case class ResolutionAmbiguous(candidates: List[ResolvedName])
      extends ResolutionError

  /** A resolution error due to the symbol not being found.
    */
  case object ResolutionNotFound extends ResolutionError

  /** A metadata-friendly storage for resolutions */
  case class Resolution(target: ResolvedName) extends IRPass.Metadata {

    /** The name of the metadata as a string. */
    override val metadataName: String = "Resolution"

    /** @inheritdoc */
    override def prepareForSerialization(compiler: Compiler): Resolution =
      this.copy(target = this.target.toAbstract)

    /** @inheritdoc */
    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[Resolution] = {
      val moduleMap = compiler.context.getPackageRepository.getModuleMap
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
    override def duplicate(): Option[IRPass.Metadata] = Some(this)
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
      override def toAbstract: Abstract =
        ModuleReference.Abstract(module.getName)

      /** @inheritdoc */
      override def unsafeAsModule(message: String = ""): Module = module

      /** @inheritdoc */
      private def writeObject(@unused stream: ObjectOutputStream): Unit = {
        throw new CompilerError(
          s"Attempting to serialize a concrete module reference to `$getName`."
        )
      }
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
