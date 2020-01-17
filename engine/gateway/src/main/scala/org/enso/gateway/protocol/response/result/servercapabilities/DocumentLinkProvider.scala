package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** The server provides document link support. */
case class DocumentLinkProvider()
object DocumentLinkProvider {
  implicit val serverCapabilitiesDocumentLinkProviderEncoder
    : Encoder[DocumentLinkProvider] =
    deriveEncoder
}
