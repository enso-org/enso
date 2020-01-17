package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides document symbol support. */
case class DocumentSymbolProvider()
object DocumentSymbolProvider {
  implicit val serverCapabilitiesDocumentSymbolProviderEncoder
    : Encoder[DocumentSymbolProvider] =
    deriveEncoder
}
