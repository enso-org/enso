package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/hover` request. */
case class Hover()
object Hover {
  implicit val clientCapabilitiesTextDocumentHoverDecoder: Decoder[Hover] =
    deriveDecoder
}
