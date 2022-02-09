package org.enso.librarymanager.published.cache

import java.nio.file.{Files, Path}

import nl.gn0s1s.bump.SemVer
import org.enso.editions.LibraryName
import org.enso.librarymanager.published.repository.LibraryManifest
import org.enso.yaml.YamlHelper

import scala.util.Try

case class CachedLibrary(
  cacheRoot: Path,
  libraryName: LibraryName,
  version: SemVer
) {

  def path: Path =
    LibraryCache.resolvePath(cacheRoot, libraryName, version)
}

object CachedLibrary {

  implicit class CachedLibraryAccess(cachedLibrary: CachedLibrary) {

    /** Get the library manifest.
      *
      * @return the library manifest, if the manifest file exists and `None` otherwise.
      */
    def getManifest: Option[Try[LibraryManifest]] = {
      val manifestPath = cachedLibrary.path.resolve(LibraryManifest.filename)
      Option.when(Files.exists(manifestPath)) {
        YamlHelper.load[LibraryManifest](manifestPath)
      }
    }
  }
}
