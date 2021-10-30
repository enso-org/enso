package org.enso.librarymanager.published.bundles

import com.typesafe.scalalogging.Logger
import nl.gn0s1s.bump.SemVer
import org.enso.editions.LibraryName
import org.enso.librarymanager.published.cache.{
  LibraryCache,
  ReadOnlyLibraryCache
}
import org.enso.logger.masking.MaskedPath

import java.nio.file.{Files, Path}

/** Implements a read-only cache backed by a repository on the local filesystem.
  *
  * This repository is either immutable (like the bundles) or user-managed (for
  * custom solutions). In the first case synchronization is not necessary, in
  * the second case it is impossible (as we have no authority over user's
  * actions). So this class performs no synchronization and in the second case
  * it is the user's case to not import libraries that are in the middle of
  * being copied into this repository.
  *
  * Usually, this implementation should not be used for the primary cache, as
  * other processes can concurrently access it, so the access should be
  * synchronized.
  */
class LocalReadOnlyRepository(root: Path) extends ReadOnlyLibraryCache {
  private val logger = Logger[LocalReadOnlyRepository]

  /** @inheritdoc */
  override def findCachedLibrary(
    libraryName: LibraryName,
    version: SemVer
  ): Option[Path] = {
    val path = LibraryCache.resolvePath(root, libraryName, version)
    if (Files.exists(path)) {
      logger.trace(
        s"$libraryName found at [${MaskedPath(path).applyMasking()}]."
      )
      Some(path)
    } else {
      logger.trace(
        s"Did not find $libraryName at [${MaskedPath(path).applyMasking()}]."
      )
      None
    }
  }
}
