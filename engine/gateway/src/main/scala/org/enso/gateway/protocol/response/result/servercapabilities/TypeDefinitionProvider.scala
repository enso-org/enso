package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides goto type definition support. */
case class TypeDefinitionProvider()
object TypeDefinitionProvider {
  implicit val serverCapabilitiesTypeDefinitionProviderEncoder
    : Encoder[TypeDefinitionProvider] =
    deriveEncoder
}
