package org.enso.librarymanager

import org.enso.editions.{Editions, LibraryName}
import org.enso.librarymanager.local.LocalLibraryProvider

import java.nio.file.Path

case class LibraryResolver(
  localLibraryProvider: LocalLibraryProvider
) {
  def resolveLibraryVersion(
    libraryName: LibraryName,
    edition: Editions.ResolvedEdition
  ): Either[LibraryResolutionError, LibraryVersion] = {
    // TODO [RW] how should prefer local libraries from parent behave in derived editions
    val localInstance = localLibraryProvider.findLibrary(libraryName)
    (edition.preferLocalLibraries, localInstance) match {
      case (true, Some(path)) => Right(LocalVersion(path))
      case _ =>
        import Editions.Resolved._
        val immediateResult =
          edition.libraries.get(libraryName.qualifiedName).map {
            case LocalLibrary(_) =>
              localInstance match {
                case Some(path) => Right(LocalVersion(path))
                case None =>
                  Left(
                    LibraryResolutionError(
                      s"Edition configuration forces to use the local version, " +
                      s"but the `$libraryName` library is not present among " +
                      s"local libraries."
                    )
                  )
              }
            case PublishedLibrary(_, version, repository) =>
              Right(PublishedVersion(version, repository))
          }

        immediateResult.getOrElse {
          edition.parent match {
            case Some(parentEdition) =>
              resolveLibraryVersion(libraryName, parentEdition)
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
}
