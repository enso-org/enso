package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides goto implementation support. */
case class ImplementationProvider()
object ImplementationProvider {
  implicit val serverCapabilitiesImplementationProviderEncoder
    : Encoder[ImplementationProvider] =
    deriveEncoder
}
