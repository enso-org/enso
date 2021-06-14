package org.enso.editions

import cats.implicits._
import org.enso.editions.EditionResolutionError.{
  EditionResolutionCycle,
  LibraryReferencesUndefinedRepository
}
import org.enso.editions.Editions.{RawEdition, ResolvedEdition}
import org.enso.editions.provider.EditionProvider

import scala.annotation.tailrec

case class EditionResolver(provider: EditionProvider) {
  def resolve(
    edition: RawEdition
  ): Either[EditionResolutionError, ResolvedEdition] =
    resolveEdition(edition, Nil)

  private def resolveEdition(
    edition: RawEdition,
    visitedEditions: List[String]
  ): Either[EditionResolutionError, ResolvedEdition] =
    for {
      parent <- resolveParent(edition.parent, visitedEditions)
      libraries <- resolveLibraries(
        edition.libraries,
        edition.repositories,
        parent
      )
    } yield Editions.Resolved.Edition(
      parent        = parent,
      engineVersion = edition.engineVersion,
      repositories  = edition.repositories,
      libraries     = libraries
    )

  private def resolveLibraries(
    libraries: Map[String, Editions.Raw.Library],
    currentRepositories: Map[String, Editions.Repository],
    parent: Option[ResolvedEdition]
  ): Either[
    LibraryReferencesUndefinedRepository,
    Map[String, Editions.Resolved.Library]
  ] = {
    val resolvedPairs: Either[
      LibraryReferencesUndefinedRepository,
      List[(String, Editions.Resolved.Library)]
    ] =
      libraries.toList.traverse { case (name, library) =>
        val resolved = resolveLibrary(library, currentRepositories, parent)
        resolved.map((name, _))
      }

    resolvedPairs.map(Map.from)
  }

  @tailrec
  private def resolveLibrary(
    library: Editions.Raw.Library,
    currentRepositories: Map[String, Editions.Repository],
    parent: Option[ResolvedEdition]
  ): Either[
    LibraryReferencesUndefinedRepository,
    Editions.Resolved.Library
  ] = library match {
    case Editions.Raw.LocalLibrary(qualifiedName) =>
      Right(Editions.Resolved.LocalLibrary(qualifiedName))
    case Editions.Raw.PublishedLibrary(
          qualifiedName,
          version,
          repositoryName
        ) =>
      (currentRepositories.get(repositoryName), parent) match {
        case (Some(repository), _) =>
          Right(
            Editions.Resolved
              .PublishedLibrary(qualifiedName, version, repository)
          )
        case (None, Some(parentEdition)) =>
          resolveLibrary(
            library,
            parentEdition.repositories,
            parentEdition.parent
          )
        case (None, None) =>
          Left(
            LibraryReferencesUndefinedRepository(
              libraryName    = library.qualifiedName,
              repositoryName = repositoryName
            )
          )
      }
  }

  private def resolveParent(
    parent: Option[String],
    visitedEditions: List[String]
  ): Either[EditionResolutionError, Option[ResolvedEdition]] = parent match {
    case Some(parentName) =>
      if (visitedEditions.contains(parentName)) {
        val cycle = parentName :: visitedEditions
        Left(EditionResolutionCycle(cycle.reverse))
      } else
        for {
          rawParent <- provider
            .findEditionForName(parentName)
            .toEither
            .left
            .map(EditionResolutionError.CannotLoadParentEdition)
          res <- resolveEdition(rawParent, parentName :: visitedEditions)
        } yield Some(res)
    case None => Right(None)
  }
}
