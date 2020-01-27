package org.enso.gateway.protocol.response.result.servercapabilities.workspace

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** @see
  * [[org.enso.gateway.protocol.response.result.servercapabilities.Workspace]].
  */
case class WorkspaceFoldersServerCapabilities(
  supported: Option[Boolean]                       = None,
  changeNotifications: Option[ChangeNotifications] = None
)
object WorkspaceFoldersServerCapabilities {
  implicit val workspaceFoldersServerCapabilitiesEncoder
    : Encoder[WorkspaceFoldersServerCapabilities] =
    deriveEncoder
}
