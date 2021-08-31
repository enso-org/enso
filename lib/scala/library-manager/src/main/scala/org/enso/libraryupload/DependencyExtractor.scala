package org.enso.libraryupload

import org.enso.editions.LibraryName
import org.enso.pkg.Package

trait DependencyExtractor[F] {
  def findDependencies(pkg: Package[F]): Set[LibraryName]
}
