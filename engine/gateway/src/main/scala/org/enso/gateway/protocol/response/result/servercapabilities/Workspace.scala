package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** Workspace specific
  * [[org.enso.gateway.protocol.response.result.ServerCapabilities]].
  */
case class Workspace()
object Workspace {
  implicit val serverCapabilitiesWorkspaceEncoder: Encoder[Workspace] =
    deriveEncoder
}
