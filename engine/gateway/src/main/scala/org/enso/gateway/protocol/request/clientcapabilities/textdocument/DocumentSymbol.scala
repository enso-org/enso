package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/documentSymbol` request. */
case class DocumentSymbol()
object DocumentSymbol {
  implicit val clientCapabilitiesTextDocumentSymbolDecoder
    : Decoder[DocumentSymbol] =
    deriveDecoder
}
