package org.enso.librarymanager.published.cache

import nl.gn0s1s.bump.SemVer
import org.enso.editions.{Editions, LibraryName, LibraryVersion}

import java.nio.file.Path
import scala.util.Try

/** A library cache that is also capable of downloading missing libraries (which
  * will then be cached).
  */
trait LibraryCache extends ReadOnlyLibraryCache {

  /** Returns the path to the library it is already cached.
    *
    * This method should not attempt to download the library if it is missing,
    * because other providers may have it.
    *
    * As this repository is not immutable - new libraries may be added during
    * the runtime, this method must too be aware of the concurrency - it should
    * use locks to make sure that, if the library is currently being installed,
    * it is not returned before the installation is complete (as otherwise the
    * runtime could access an incompletely installed library which could lead to
    * errors).
    */
  override def findCachedLibrary(
    libraryName: LibraryName,
    version: SemVer
  ): Option[Path]

  /** If the cache contains the library, it is returned immediately, otherwise,
    * it tries to download the missing library.
    *
    * @param libraryName the name of the library to search for
    * @param version the library version
    * @param recommendedRepository the repository that should be used to
    *                              download the library from, if it is missing
    * @param dependencyResolver a function that will specify what versions of
    *                           dependencies should be also downloaded when
    *                           installing the missing library (if any)
    *                           TODO [RW] the design of this function should be refined in #1772
    * @return the path to the library or a failure if the library could not be
    *         installed
    */
  def findOrInstallLibrary(
    libraryName: LibraryName,
    version: SemVer,
    recommendedRepository: Editions.Repository,
    dependencyResolver: LibraryName => Option[LibraryVersion]
  ): Try[Path]
}

object LibraryCache {

  /** Finds a path to a particular library version inside of a local
    * repository/cache according to the cache's directory structure.
    *
    * @param root path to the root of the repository
    * @param libraryName name of the library
    * @param version library version
    * @return the path at which the specified library would be located in the
    *         repository
    */
  def resolvePath(root: Path, libraryName: LibraryName, version: SemVer): Path =
    root
      .resolve(libraryName.namespace)
      .resolve(libraryName.name)
      .resolve(version.toString)
}

// TODO [RW] move this elsewhere
/* Note [Library Cache Concurrency Model]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The library cache may be used by multiple instances of the engine and other
 * tools running concurrently and so it needs to handle concurrent access
 * scenarios.
 *
 * Currently our tools do not provide a way to uninstall libraries from the
 * cache, which will simplify the logic significantly, in the future it may be
 * extended to allow for clearing the cache. (Currently the user can just
 * manually clean the cache directory when no instances are running.)
 *
 * Thanks to the mentioned assumption, once a library is present in the cache,
 * we can assume that it will not disappear, so we do not need to synchronize
 * read access. The only thing that needs to be synchronized is installing
 * libraries - to make sure that if two processes try to install the same
 * library, only one of them actually performs the action. We also need to be
 * sure that when one process checks if the library exists, and if another
 * process is in the middle of installing it, it will not yet report it as
 * existing (as this could lead to loading an only-partially installed library).
 * The primary way of ensuring that will be to install the libraries to a
 * temporary cache directory next to the true cache and atomically move it at
 * the end of the operation. However as we do not have real guarantees that the
 * filesystem move is atomic (although most of the time it should be if it is
 * within a single filesystem), we will use locking to ensure consistency.
 * Obviously, every client that tries to install a library will acquire a write
 * lock for it, so that only one client is actually installing; but also every
 * client checking for the existence of the library will briefly acquire a read
 * lock for that library - thus if the library is currently being installed, the
 * client will need to wait for this read lock until the library installation is
 * finished and so will never encounter it in a 'partially installed' state. If
 * the library is not installed, the client can release the read lock and
 * re-acquire the write lock to try to install it. After acquiring the write
 * lock, it should check again if the library is available, to make sure that no
 * other process installed it in the meantime. This solution is efficient
 * because every library is locked independently and read locks can be acquired
 * by multiple clients at the same time, so the synchronization overhead for
 * already installed libraries is negligible, and for libraries that need to be
 * installed it is too negligible in comparison to the time required to download
 * the libraries.
 *
 * A single LockManager (and its locks directory) should be associated with at
 * most one library cache directory, as it makes sense for the distribution to
 * have only one cache, so the lock entries are not disambiguated in any way.
 */
