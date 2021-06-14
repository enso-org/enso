package org.enso.librarymanager

import org.enso.editions.{Editions, LibraryName}
import org.enso.librarymanager.local.LocalLibraryProvider

case class LibraryResolver(
  localLibraryProvider: LocalLibraryProvider
) {

  def resolveLibraryVersion(
    libraryName: LibraryName,
    edition: Editions.ResolvedEdition,
    preferLocalLibraries: Boolean
  ): Either[LibraryResolutionError, LibraryVersion] = {
    if (preferLocalLibraries) {
      localLibraryProvider
        .findLibrary(libraryName)
        .map(_ => Right(LibraryVersion.Local))
        .getOrElse {
          resolveLibraryFromEdition(libraryName, edition)
        }
    } else resolveLibraryFromEdition(libraryName, edition)
  }

  def resolveLibraryFromEdition(
    libraryName: LibraryName,
    edition: Editions.ResolvedEdition
  ): Either[LibraryResolutionError, LibraryVersion] = {
    import Editions.Resolved._
    val immediateResult =
      edition.libraries.get(libraryName.qualifiedName).map {
        case LocalLibrary(_) =>
          Right(LibraryVersion.Local)
        case PublishedLibrary(_, version, repository) =>
          Right(LibraryVersion.Published(version, repository))
      }

    immediateResult.getOrElse {
      edition.parent match {
        case Some(parentEdition) =>
          resolveLibraryFromEdition(libraryName, parentEdition)
        case None =>
          Left(
            LibraryResolutionError(
              s"The library `$libraryName` is not defined within " +
              s"the edition."
            )
          )
      }
    }
  }
}
