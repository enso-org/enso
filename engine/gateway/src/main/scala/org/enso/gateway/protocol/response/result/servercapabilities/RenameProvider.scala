package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides rename support. RenameOptions may only be specified if
  * the client states that it supports `prepareSupport` in its initial
  * `initialize` request.
  */
case class RenameProvider()
object RenameProvider {
  implicit val serverCapabilitiesRenameProviderEncoder
    : Encoder[RenameProvider] =
    deriveEncoder
}
