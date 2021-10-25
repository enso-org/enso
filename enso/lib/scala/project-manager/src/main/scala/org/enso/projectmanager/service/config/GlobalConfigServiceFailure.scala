package org.enso.projectmanager.service.config

import org.enso.jsonrpc
import org.enso.projectmanager.protocol.ProjectManagementApi
import org.enso.projectmanager.requesthandler.FailureMapper

/** Represents a failure of the Global Config Service. */
sealed trait GlobalConfigServiceFailure

object GlobalConfigServiceFailure {

  /** Singals that the configuration file could not have been accessed (read or
    * written).
    */
  case class ConfigurationFileAccessFailure(message: String)
      extends GlobalConfigServiceFailure

  /** [[FailureMapper]] instance for [[GlobalConfigServiceFailure]]. */
  implicit val failureMapper = new FailureMapper[GlobalConfigServiceFailure] {
    override def mapFailure(
      failure: GlobalConfigServiceFailure
    ): jsonrpc.Error = failure match {
      case ConfigurationFileAccessFailure(message) =>
        ProjectManagementApi.GlobalConfigurationAccessError(message)
    }
  }
}
