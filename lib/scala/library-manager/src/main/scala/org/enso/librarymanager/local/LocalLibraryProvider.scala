package org.enso.librarymanager.local

import org.enso.distribution.DistributionManager
import org.enso.editions.LibraryName

import java.nio.file.Path

/** A provider for local libraries. */
trait LocalLibraryProvider {

  /** Returns the path to a local instance of the requested library, if it is
    * available.
    */
  def findLibrary(libraryName: LibraryName): Option[Path]
}

object LocalLibraryProvider {

  /** Creates a default [[LocalLibraryProvider]] from a [[DistributionManager]].
    */
  def make(distributionManager: DistributionManager): LocalLibraryProvider =
    new DefaultLocalLibraryProvider(distributionManager)
}
