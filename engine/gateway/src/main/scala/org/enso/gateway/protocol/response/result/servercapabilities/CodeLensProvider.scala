package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides code lens. */
case class CodeLensProvider()
object CodeLensProvider {
  implicit val serverCapabilitiesCodeLensProviderEncoder
    : Encoder[CodeLensProvider] =
    deriveEncoder
}
