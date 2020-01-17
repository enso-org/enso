package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides document formatting on typing. */
case class DocumentOnTypeFormattingProvider()
object DocumentOnTypeFormattingProvider {
  implicit val serverCapabilitiesDocumentOnTypeFormattingProviderEncoder
    : Encoder[DocumentOnTypeFormattingProvider] =
    deriveEncoder
}
