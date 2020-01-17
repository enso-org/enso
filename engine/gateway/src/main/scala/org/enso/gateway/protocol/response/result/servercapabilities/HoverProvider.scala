package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides hover support. */
case class HoverProvider()
object HoverProvider {
  implicit val serverCapabilitiesHoverProviderEncoder: Encoder[HoverProvider] =
    deriveEncoder
}
