package org.enso.compiler.data

import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.Module

/**
  * A utility structure for resolving symbols in a given module.
  *
  * @param types the types defined in the current module
  * @param currentModule the module holding these bindings
  */
case class BindingsMap(
  types: List[BindingsMap.Cons],
  currentModule: Module
) extends IRPass.Metadata {
  import BindingsMap._

  override val metadataName: String = "Bindings Map"

  override def duplicate(): Option[IRPass.Metadata] = Some(this)

  /**
    * Other modules, imported by [[currentModule]].
    */
  var resolvedImports: List[Module] = List()

  private def findConstructorCandidates(
    name: String
  ): List[ResolvedConstructor] = {
    types
      .filter(_.name == name)
      .map(ResolvedConstructor(currentModule, _))
  }

  private def findLocalCandidates(name: String): List[ResolvedName] = {
    if (currentModule.getName.item == name) {
      List(ResolvedModule(currentModule))
    } else {
      findConstructorCandidates(name)
    }
  }

  private def findQualifiedImportCandidates(
    name: String
  ): List[ResolvedName] = {
    resolvedImports.filter(_.getName.item == name).map(ResolvedModule)
  }

  private def findExportedCandidatesInImports(
    name: String
  ): List[ResolvedName] = {
    resolvedImports
      .map { mod =>
        mod.getIr.unsafeGetMetadata(
          BindingAnalysis,
          "Wrong pass ordering. Running resolution on an unparsed module"
        )
      }
      .flatMap(_.findConstructorCandidates(name))
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
}

object BindingsMap {

  /**
    * A representation of a constructor.
    *
    * @param name the name of the constructor.
    * @param arity the number of fields in the constructor.
    */
  case class Cons(name: String, arity: Int)

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

  /** A metadata-friendly storage for resolutions  */
  case class Resolution(target: ResolvedName)
    extends IRPass.Metadata {

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
