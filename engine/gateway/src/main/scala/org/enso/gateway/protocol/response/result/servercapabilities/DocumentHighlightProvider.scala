package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides document highlight support. */
case class DocumentHighlightProvider()
object DocumentHighlightProvider {
  implicit val serverCapabilitiesDocumentHighlightProviderEncoder
    : Encoder[DocumentHighlightProvider] =
    deriveEncoder
}
