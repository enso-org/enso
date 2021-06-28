package org.enso.librarymanager.published.repository

import nl.gn0s1s.bump.SemVer
import org.enso.downloader.http.URIBuilder
import org.enso.editions.Editions.Repository
import org.enso.editions.LibraryName

object RepositoryHelper {
  implicit class RepositoryMethods(val repository: Repository) {
    def resolveLibraryRoot(name: LibraryName, version: SemVer): URIBuilder =
      URIBuilder
        .fromUri(repository.url)
        .addPathSegment(name.namespace)
        .addPathSegment(name.name)
        .addPathSegment(version.toString)
  }
}
