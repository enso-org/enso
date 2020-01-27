package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/rangeFormatting` request. */
case class RangeFormatting(
  dynamicRegistration: Option[Boolean] = None
)
object RangeFormatting {
  implicit val clientCapabilitiesTextDocumentRangeFormattingDecoder
    : Decoder[RangeFormatting] =
    deriveDecoder
}
