package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides color provider support. */
case class ColorProvider()
object ColorProvider {
  implicit val serverCapabilitiesColorProviderEncoder: Encoder[ColorProvider] =
    deriveEncoder
}
