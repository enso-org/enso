package org.enso.librarymanager.published

import nl.gn0s1s.bump.SemVer
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.editions.LibraryName

import java.nio.file.{Files, Path}

class LibraryCache(root: Path) {
  def getLibrary(libraryName: LibraryName, version: SemVer): Option[Path] = {
    val path = LibraryCache.resolvePath(root, libraryName, version)
    if (Files.exists(path)) Some(path)
    else None
  }
}

object LibraryCache {
  def resolvePath(
    cacheRoot: Path,
    libraryName: LibraryName,
    libraryVersion: SemVer
  ): Path =
    cacheRoot / libraryName.namespace / libraryName.name / libraryVersion.toString
}
