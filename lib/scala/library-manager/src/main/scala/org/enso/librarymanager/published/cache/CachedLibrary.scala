package org.enso.librarymanager.published.cache

import java.nio.file.{Files, Path}

import nl.gn0s1s.bump.SemVer
import org.enso.editions.LibraryName
import org.enso.librarymanager.published.repository.LibraryManifest
import org.enso.pkg.{Config, Package}
import org.enso.yaml.YamlHelper

import scala.util.Try

/** The library that was cached.
  *
  * @param cacheRoot the root cache directory
  * @param libraryName the library name
  * @param version the library version
  */
case class CachedLibrary(
  cacheRoot: Path,
  libraryName: LibraryName,
  version: SemVer
) {

  /** The filepath to the library. */
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

    /** Get the library package config.
      *
      * @return the parsed library config.
      */
    def getPackage: Try[Config] = {
      val configPath = cachedLibrary.path.resolve(Package.configFileName)
      YamlHelper.load[Config](configPath)
    }
  }
}
