package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides document formatting. */
case class DocumentFormattingProvider()
object DocumentFormattingProvider {
  implicit val serverCapabilitiesDocumentFormattingProviderEncoder
    : Encoder[DocumentFormattingProvider] =
    deriveEncoder
}
