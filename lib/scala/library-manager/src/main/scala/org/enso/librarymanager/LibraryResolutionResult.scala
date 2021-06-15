package org.enso.librarymanager

import org.enso.cli.task.TaskProgress

import java.nio.file.Path

/** Encapsulates possible results of library resolution. */
sealed trait LibraryResolutionResult
object LibraryResolutionResult {

  /** Indicates that the resolution has failed immediately with an error. */
  case class ResolutionFailure(error: Throwable) extends LibraryResolutionResult

  /** Indicates that the resolution has succeeded immediately.
    *
    * The library was already available on disk and the path to it is returned.
    */
  case class ResolvedImmediately(path: Path) extends LibraryResolutionResult

  /** Indicates that the initial library resolution has succeeded (that the
    * corrrect version could be inferred from the edition) but the library was
    * not installed, so it must be downloaded.
    *
    * It contains a [[TaskProgress]] instance that can track the progress of the
    * download and will be completed once the library has been installed.
    *
    * The download and installation may of course fail as well, which is
    * indicated by failure of the [[TaskProgress]].
    */
  case class ResolutionPending(result: TaskProgress[Path])
      extends LibraryResolutionResult
}
