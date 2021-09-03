package org.enso.libraryupload

import org.enso.editions.LibraryName
import org.enso.pkg.Package

/** A general interface for a helper that allows to extract dependencies from
  * the project.
  */
trait DependencyExtractor[F] {

  /** Finds dependencies of a given project package. */
  def findDependencies(pkg: Package[F]): Set[LibraryName]
}
