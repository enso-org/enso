package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/foldingRange` request. */
case class FoldingRange()
object FoldingRange {
  implicit val clientCapabilitiesTextDocumentFoldingRangeDecoder
    : Decoder[FoldingRange] =
    deriveDecoder
}
