package org.enso.librarymanager

import org.enso.cli.task.TaskProgress

import java.nio.file.Path

sealed trait LibraryResolutionResult
object LibraryResolutionResult {
  case class ResolutionFailure(error: Throwable) extends LibraryResolutionResult
  case class ResolvedImmediately(path: Path)     extends LibraryResolutionResult
  case class ResolutionPending(result: TaskProgress[Path])
      extends LibraryResolutionResult
}
