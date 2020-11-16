package org.enso.projectmanager.requesthandler

import org.enso.jsonrpc.Error
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.projectmanager.protocol.ProjectManagementApi._
import org.enso.projectmanager.service.ProjectServiceFailure
import org.enso.projectmanager.service.ProjectServiceFailure.{
  ProjectNotOpen,
  ProjectOpenByOtherPeers,
  _
}

object ProjectServiceFailureMapper {

  /** Maps project service failures to JSON RPC errors.
    */
  val mapFailure: ProjectServiceFailure => Error = {
    case ValidationFailure(msg)     => ProjectNameValidationError(msg)
    case DataStoreFailure(msg)      => ProjectDataStoreError(msg)
    case ProjectExists              => ProjectExistsError
    case ProjectNotFound            => ProjectNotFoundError
    case ProjectOpenFailed(msg)     => ProjectOpenError(msg)
    case ProjectCloseFailed(msg)    => ProjectCloseError(msg)
    case ProjectNotOpen             => ProjectNotOpenError
    case ProjectOpenByOtherPeers    => ProjectOpenByOtherPeersError
    case CannotRemoveOpenProject    => CannotRemoveOpenProjectError
    case ProjectOperationTimeout    => ServiceError
    case LanguageServerFailure(msg) => LanguageServerError(msg)
    case ProjectManagerUpgradeRequiredFailure(version) =>
      ProjectManagerUpgradeRequired(version)
    case MissingComponentFailure(msg)      => MissingComponentError(msg)
    case BrokenComponentFailure(msg)       => BrokenComponentError(msg)
    case ComponentInstallationFailure(msg) => ComponentInstallationError(msg)
    case ComponentRepositoryAccessFailure(msg) =>
      ComponentRepositoryUnavailable(msg)
    case ComponentUninstallationFailure(msg) =>
      ComponentUninstallationError(msg)
  }

  /** [[FailureMapper]] instance for [[ProjectServiceFailure]]. */
  implicit val failureMapper: FailureMapper[ProjectServiceFailure] =
    (failure: ProjectServiceFailure) => mapFailure(failure)
}
