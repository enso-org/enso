package org.enso.compiler.data

import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.Module

/**
  * A utility structure for resolving symbols in a given module.
  *
  * @param types the types defined in the current module
  * @param polyglotSymbols the polyglot symbols imported into the scope
  * @param moduleMethods the methods defined with current module as `this`
  * @param currentModule the module holding these bindings
  */
case class BindingsMap(
  types: List[BindingsMap.Cons],
  polyglotSymbols: List[BindingsMap.PolyglotSymbol],
  moduleMethods: List[BindingsMap.ModuleMethod],
  currentModule: Module
) extends IRPass.Metadata {
  import BindingsMap._

  override val metadataName: String = "Bindings Map"

  override def duplicate(): Option[IRPass.Metadata] = Some(this)

  /**
    * Other modules, imported by [[currentModule]].
    */
  var resolvedImports: List[ResolvedImport] = List()

  private def findConstructorCandidates(
    name: String
  ): List[ResolvedConstructor] = {
    types
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

  private def findMethodCandidates(name: String): List[ResolvedName] = {
    moduleMethods
      .filter(_.name.toLowerCase == name.toLowerCase)
      .map(ResolvedMethod(currentModule, _))
  }

  private def findLocalCandidates(name: String): List[ResolvedName] = {
    if (currentModule.getName.item == name) {
      List(ResolvedModule(currentModule))
    } else {
      val conses   = findConstructorCandidates(name)
      val polyglot = findPolyglotCandidates(name)
      val methods  = findMethodCandidates(name)
      conses ++ polyglot ++ methods
    }
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
          imp.module.getIr
            .unsafeGetMetadata(
              BindingAnalysis,
              "Wrong pass ordering. Running resolution on an unparsed module."
            )
            .findExportedSymbolsFor(name)
        } else { List() }
      }
  }

  private def handleAmbiguity(
    candidates: List[ResolvedName]
  ): Either[ResolutionError, ResolvedName] = {
    candidates match {
      case List()   => Left(ResolutionNotFound)
      case List(it) => Right(it)
      case items    => Left(ResolutionAmbiguous(items))
    }
  }

  private def getBindingsFrom(module: Module): BindingsMap = {
    module.getIr.unsafeGetMetadata(
      BindingAnalysis,
      "imported module has no binding map info"
    )
  }

  /**
    * Resolves a name in the context of current module.
    *
    * @param name the name to resolve.
    * @return a resolution for [[name]] or an error, if the name could not be
    *         resolved.
    */
  def resolveUppercaseName(
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

  /**
    * Resolves a qualified name to a symbol in the context of this module.
    *
    * @param name the name to resolve
    * @return a resolution for `name`
    */
  def resolveQualifiedName(
    name: List[String]
  ): Either[ResolutionError, ResolvedName] =
    name match {
      case List()     => Left(ResolutionNotFound)
      case List(item) => resolveUppercaseName(item)
      case List(module, cons) =>
        resolveUppercaseName(module).flatMap {
          case ResolvedModule(mod) =>
            getBindingsFrom(mod).resolveExportedName(cons)
          case _ => Left(ResolutionNotFound)
        }
      case _ =>
        // TODO[MK] Implement when exports possible. Currently this has
        // no viable interpretation.
        Left(ResolutionNotFound)
    }

  private def findExportedSymbolsFor(name: String): List[ResolvedName] = {
    val matchingConses = types
      .filter(_.name.toLowerCase == name.toLowerCase)
      .map(ResolvedConstructor(currentModule, _))
    val matchingMethods = moduleMethods
      .filter(_.name.toLowerCase == name.toLowerCase)
      .map(ResolvedMethod(currentModule, _))
    matchingConses ++ matchingMethods
  }

  /**
    * Resolves a name exported by this module.
    *
    * @param name the name to resolve
    * @return the resolution for `name`
    */
  def resolveExportedName(
    name: String
  ): Either[ResolutionError, ResolvedName] = {
    handleAmbiguity(findExportedSymbolsFor(name))
  }
}

object BindingsMap {

  /**
    * A representation of a resolved import statement.
    *
    * @param importDef the definition of the import
    * @param module the module this import resolves to
    */
  case class ResolvedImport(
    importDef: IR.Module.Scope.Import.Module,
    module: Module
  )

  /**
    * A representation of a constructor.
    *
    * @param name the name of the constructor.
    * @param arity the number of fields in the constructor.
    */
  case class Cons(name: String, arity: Int)

  /**
    * A representation of an imported polyglot symbol.
    *
    * @param name the name of the symbol.
    */
  case class PolyglotSymbol(name: String)

  /**
    * A representation of a method defined on the current module.
    *
    * @param name the name of the method.
    */
  case class ModuleMethod(name: String)

  /**
    * A result of successful name resolution.
    */
  sealed trait ResolvedName

  /**
    * A representation of a name being resolved to a constructor.
    *
    * @param definitionModule the module the constructor is defined in.
    * @param cons a representation of the constructor.
    */
  case class ResolvedConstructor(definitionModule: Module, cons: Cons)
      extends ResolvedName

  /**
    * A representation of a name being resolved to a module.
    *
    * @param module the module the name resolved to.
    */
  case class ResolvedModule(module: Module) extends ResolvedName

  /**
    * A representation of a name being resolved to a method call.
    * @param module the module defining the method.
    * @param method the method representation.
    */
  case class ResolvedMethod(module: Module, method: ModuleMethod)
      extends ResolvedName

  /**
    * A representation of a name being resolved to a polyglot symbol.
    *
    * @param symbol the imported symbol name.
    */
  case class ResolvedPolyglotSymbol(module: Module, symbol: PolyglotSymbol)
      extends ResolvedName

  /**
    * A representation of an error during name resolution.
    */
  sealed trait ResolutionError

  /**
    * A representation of a resolution error due to symbol ambiguity.
    *
    * @param candidates all the possible resolutions for the name.
    */
  case class ResolutionAmbiguous(candidates: List[ResolvedName])
      extends ResolutionError

  /**
    * A resolution error due to the symbol not being found.
    */
  case object ResolutionNotFound extends ResolutionError

  /** A metadata-friendly storage for resolutions */
  case class Resolution(target: ResolvedName) extends IRPass.Metadata {

    /** The name of the metadata as a string. */
    override val metadataName: String = "Resolution"

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
}
