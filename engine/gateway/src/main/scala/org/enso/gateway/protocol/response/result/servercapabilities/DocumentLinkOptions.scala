package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** Server capability to provide document link support. */
case class DocumentLinkOptions(
  workDoneProgress: Option[Boolean] = None,
  resolveProvider: Option[Boolean]  = None
)
object DocumentLinkOptions {
  implicit val serverCapabilitiesDocumentLinkOptionsEncoder
    : Encoder[DocumentLinkOptions] =
    deriveEncoder
}
