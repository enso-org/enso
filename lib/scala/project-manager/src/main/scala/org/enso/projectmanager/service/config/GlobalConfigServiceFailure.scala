package org.enso.projectmanager.service.config

import org.enso.jsonrpc
import org.enso.projectmanager.protocol.ProjectManagementApi
import org.enso.projectmanager.requesthandler.FailureMapper

sealed trait GlobalConfigServiceFailure

object GlobalConfigServiceFailure {
  case class ConfigurationFileAccessFailure(message: String)
      extends GlobalConfigServiceFailure

  implicit val failureMapper = new FailureMapper[GlobalConfigServiceFailure] {
    override def mapFailure(
      failure: GlobalConfigServiceFailure
    ): jsonrpc.Error = failure match {
      case ConfigurationFileAccessFailure(message) =>
        ProjectManagementApi.GlobalConfigurationAccessError(message)
    }
  }
}
