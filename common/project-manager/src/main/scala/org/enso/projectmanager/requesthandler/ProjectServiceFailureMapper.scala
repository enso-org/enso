package org.enso.projectmanager.requesthandler

import org.enso.jsonrpc.Error
import org.enso.projectmanager.protocol.ProjectManagementApi._
import org.enso.projectmanager.service.ProjectServiceFailure
import org.enso.projectmanager.service.ProjectServiceFailure.{
  DataStoreFailure,
  ProjectExists,
  ProjectNotFound,
  ValidationFailure
}

object ProjectServiceFailureMapper {

  /**
    * Maps project service failures to JSON RPC errors.
    */
  val mapFailure: ProjectServiceFailure => Error = {
    case ValidationFailure(msg) => ProjectNameValidationError(msg)
    case DataStoreFailure(msg)  => ProjectDataStoreError(msg)
    case ProjectExists          => ProjectExistsError
    case ProjectNotFound        => ProjectNotFoundError
  }

}
