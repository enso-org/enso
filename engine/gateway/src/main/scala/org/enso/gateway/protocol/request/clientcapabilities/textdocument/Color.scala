package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/documentColor` and the
  * `textDocument/colorPresentation` request.
  */
case class Color()
object Color {
  implicit val clientCapabilitiesTextDocumentColorDecoder: Decoder[Color] =
    deriveDecoder
}
