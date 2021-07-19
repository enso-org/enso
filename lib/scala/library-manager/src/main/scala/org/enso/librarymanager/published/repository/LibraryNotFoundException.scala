package org.enso.librarymanager.published.repository

import nl.gn0s1s.bump.SemVer
import org.enso.editions.LibraryName

sealed class LibraryDownloadFailure(message: String)
    extends RuntimeException(message)

case class LibraryNotFoundException(
  libraryName: LibraryName,
  version: SemVer,
  uri: String
) extends LibraryDownloadFailure(
      s"Library [$libraryName:$version] was not found at [$uri]."
    )
