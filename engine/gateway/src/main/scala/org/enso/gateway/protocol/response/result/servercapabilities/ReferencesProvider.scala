package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides find references support. */
case class ReferencesProvider()
object ReferencesProvider {
  implicit val serverCapabilitiesReferencesProviderEncoder
    : Encoder[ReferencesProvider] =
    deriveEncoder
}
