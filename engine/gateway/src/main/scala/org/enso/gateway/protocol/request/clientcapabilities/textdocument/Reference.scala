package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/references` request. */
case class Reference()
object Reference {
  implicit val clientCapabilitiesTextDocumentReferenceDecoder
    : Decoder[Reference] =
    deriveDecoder
}
