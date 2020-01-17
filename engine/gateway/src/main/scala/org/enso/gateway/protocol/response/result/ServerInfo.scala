package org.enso.gateway.protocol.response.result

import io.circe.generic.semiauto.deriveEncoder
import io.circe.Encoder

/** [[org.enso.gateway.protocol.response.Result.InitializeResult]] server info.
  *
  * @param name    Name of Language Server
  * @param version Version of Language Server
  */
case class ServerInfo(
  name: String,
  version: Option[String] = None
)
object ServerInfo {
  implicit val serverInfoEncoder: Encoder[ServerInfo] = deriveEncoder
}
