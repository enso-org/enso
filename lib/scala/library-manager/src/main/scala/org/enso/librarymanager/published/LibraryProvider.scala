package org.enso.librarymanager.published

import nl.gn0s1s.bump.SemVer
import org.enso.cli.task.TaskProgress
import org.enso.editions.Editions.Repository
import org.enso.editions.LibraryName
import org.enso.librarymanager.{LibraryResolver, LibraryVersion}

import java.nio.file.Path

trait LibraryProvider {

  /** Tries to locate the requested library at a specific version.
    *
    * If the library is not present, a download may be attempted - this is where
    * the recommendedRepository is used to guide which repository should be used
    * for the download.
    *
    * If the library is available immediately, Right should be returned to
    * indicate that. Otherwise a Left containing a TaskProgress is returned
    * which will track the progress of the download (as it may take a
    * significant amount of time, especially because other dependencies may also
    * be pre-downloaded).
    */
  def findLibrary(
    libraryName: LibraryName,
    version: SemVer,
    recommendedRepository: Repository,
    dependencyResolver: LibraryName => Option[LibraryVersion]
  ): Either[TaskProgress[Path], Path]
}
