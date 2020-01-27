package org.enso.gateway.protocol.response.result

import io.circe.generic.semiauto.deriveEncoder
import io.circe.Encoder

/** Server info in
  * [[org.enso.gateway.protocol.response.Result.InitializeResult]].
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
