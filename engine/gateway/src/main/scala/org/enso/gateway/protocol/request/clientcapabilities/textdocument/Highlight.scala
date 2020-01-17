package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/documentHighlight` request. */
case class Highlight()
object Highlight {
  implicit val clientCapabilitiesTextDocumentHighlightDecoder
    : Decoder[Highlight] =
    deriveDecoder
}
