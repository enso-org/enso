package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import org.enso.gateway.protocol.response.result.servercapabilities.workspace.WorkspaceFoldersServerCapabilities

/** Workspace-specific
  * [[org.enso.gateway.protocol.response.result.ServerCapabilities]].
  */
case class Workspace(
  workspaceFolders: Option[WorkspaceFoldersServerCapabilities] = None
)
object Workspace {
  implicit val serverCapabilitiesWorkspaceEncoder: Encoder[Workspace] =
    deriveEncoder
}
