package org.enso.projectmanager.service.filesystem

import org.enso.jsonrpc.Error
import org.enso.projectmanager.requesthandler.ProjectServiceFailureMapper
import org.enso.projectmanager.service.ProjectServiceFailure

object FileSystemServiceFailureMapper {

  /** Maps filesystem service failures to JSON RPC errors.
    *
    * @param failure the file system service failure
    * @return the JSON-RPC error
    */
  def mapFailure(failure: FileSystemServiceFailure): Error =
    ProjectServiceFailureMapper.mapFailure(toProjectServiceFailure(failure))

  private def toProjectServiceFailure(
    failure: FileSystemServiceFailure
  ): ProjectServiceFailure =
    failure match {
      case FileSystemServiceFailure.FileSystem(msg) =>
        ProjectServiceFailure.DataStoreFailure(msg)

      case FileSystemServiceFailure.ProjectRepository(msg, reason) =>
        ProjectServiceFailure.DataStoreFailure(s"$msg [$reason]")
    }
}
