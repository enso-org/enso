package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides goto definition support. */
case class DefinitionProvider()
object DefinitionProvider {
  implicit val serverCapabilitiesDefinitionProviderEncoder
    : Encoder[DefinitionProvider] =
    deriveEncoder
}
