package org.enso.projectmanager.service.filesystem

import org.enso.projectmanager.requesthandler.FailureMapper

sealed trait FileSystemServiceFailure

object FileSystemServiceFailure {

  case class FileSystem(msg: String) extends FileSystemServiceFailure

  case class ProjectRepository(msg: String, reason: String)
      extends FileSystemServiceFailure

  /** [[FailureMapper]] instance for [[FileSystemServiceFailure]]. */
  implicit val failureMapper: FailureMapper[FileSystemServiceFailure] =
    (failure: FileSystemServiceFailure) =>
      FileSystemServiceFailureMapper.mapFailure(failure)

}
