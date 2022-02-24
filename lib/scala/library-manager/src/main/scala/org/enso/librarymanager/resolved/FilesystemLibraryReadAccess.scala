package org.enso.librarymanager.resolved

import org.enso.librarymanager.published.repository.LibraryManifest
import org.enso.pkg.{Config, Package}
import org.enso.yaml.YamlHelper

import java.nio.file.Files

import scala.util.Try

/** Default filesystem read access to libraries.
  *
  * @param libraryPath the library location on the filesystem
  */
class FilesystemLibraryReadAccess(libraryPath: LibraryRoot)
    extends LibraryReadAccess {

  /** @inheritdoc */
  override def readManifest(): Option[Try[LibraryManifest]] = {
    val manifestPath = libraryPath.location.resolve(LibraryManifest.filename)
    Option.when(Files.exists(manifestPath)) {
      YamlHelper.load[LibraryManifest](manifestPath)
    }
  }

  /** @inheritdoc */
  override def readPackage(): Try[Config] = {
    val configPath = libraryPath.location.resolve(Package.configFileName)
    YamlHelper.load[Config](configPath)
  }

}
