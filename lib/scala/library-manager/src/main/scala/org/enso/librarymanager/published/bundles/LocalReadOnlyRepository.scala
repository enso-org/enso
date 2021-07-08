package org.enso.librarymanager.published.bundles

import nl.gn0s1s.bump.SemVer
import org.enso.editions.LibraryName
import org.enso.librarymanager.published.cache.{
  LibraryCache,
  ReadOnlyLibraryCache
}

import java.nio.file.{Files, Path}

/** Implements a read-only cache backed by a repository on the local filesystem.
  *
  * This repository is either immutable (like the bundles) or user-managed (for
  * custom solutions). In the first case synchronization is not necessary, in
  * the second case it is impossible (as we have no authority over user's
  * actions). So this class performs no synchronization and in the second case
  * it is the user's case to not import libraries that are in the middle of
  * being copied into this repository.
  */
class LocalReadOnlyRepository(root: Path) extends ReadOnlyLibraryCache {

  /** @inheritdoc */
  override def findCachedLibrary(
    libraryName: LibraryName,
    version: SemVer
  ): Option[Path] = {
    val path = LibraryCache.resolvePath(root, libraryName, version)
    if (Files.exists(path)) Some(path) else None
  }
}
