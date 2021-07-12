package org.enso.librarymanager.published.cache
import nl.gn0s1s.bump.SemVer
import org.enso.editions.{Editions, LibraryName, LibraryVersion}

import java.nio.file.Path
import scala.util.{Failure, Try}

/** A temporary cache that provides no libraries.
  *
  * This is a temporary poly-fill which will later be replaced when the
  * downloading mechanism is implemented.
  */
class NoOpCache extends LibraryCache {

  /** @inheritdoc */
  override def findCachedLibrary(
    libraryName: LibraryName,
    version: SemVer
  ): Option[Path] = None

  /** @inheritdoc */
  override def findOrInstallLibrary(
    libraryName: LibraryName,
    version: SemVer,
    recommendedRepository: Editions.Repository,
    dependencyResolver: LibraryName => Option[LibraryVersion]
  ): Try[Path] = Failure(
    new NotImplementedError("Downloading libraries is not yet implemented.")
  )
}
