package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides document range formatting. */
case class DocumentRangeFormattingProvider()
object DocumentRangeFormattingProvider {
  implicit val serverCapabilitiesDocumentRangeFormattingProviderEncoder
    : Encoder[DocumentRangeFormattingProvider] =
    deriveEncoder
}
