package org.enso.librarymanager

import org.enso.editions.LibraryName

import java.nio.file.Path
import scala.util.Try

trait ResolvingLibraryProvider {
  def findLibrary(
    name: LibraryName
  ): Either[ResolvingLibraryProvider.Error, Path]
}

object ResolvingLibraryProvider {
  sealed trait Error
  object Error {
    case class NotResolved(details: LibraryResolutionError) extends Error
    case class DownloadFailed(reason: Throwable)            extends Error
  }
}
