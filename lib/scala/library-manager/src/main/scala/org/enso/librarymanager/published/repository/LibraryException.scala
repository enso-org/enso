package org.enso.librarymanager.published.repository

import org.enso.semver.SemVer
import org.enso.editions.LibraryName

/** Indicates that the library could not be downloaded. */
sealed class LibraryDownloadFailure(message: String)
    extends RuntimeException(message)

/** Indicates that the library was not found in the recommended repository. */
case class LibraryNotFoundException(
  libraryName: LibraryName,
  version: SemVer,
  uri: String
) extends LibraryDownloadFailure(
      s"Library [$libraryName:$version] was not found at [$uri]."
    )
