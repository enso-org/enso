package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/implementation` request. */
case class Implementation()
object Implementation {
  implicit val clientCapabilitiesTextDocumentImplementationDecoder
    : Decoder[Implementation] =
    deriveDecoder
}
