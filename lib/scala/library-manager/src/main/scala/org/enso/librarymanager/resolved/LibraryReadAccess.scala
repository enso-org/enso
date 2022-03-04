package org.enso.librarymanager.resolved

import org.enso.librarymanager.published.repository.LibraryManifest
import org.enso.pkg.Config

import scala.util.Try

/** Base trait allowing to read the library files on a filesystem. */
trait LibraryReadAccess {

  /** Read the library manifest file.
    *
    * @return the library manifest, if the manifest file exists and `None` otherwise.
    */
  def readManifest(): Option[Try[LibraryManifest]]

  /** Read the library package config.
    *
    * @return the parsed library config.
    */
  def readPackage(): Try[Config]
}
