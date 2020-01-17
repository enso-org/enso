package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/codeLens` request. */
case class CodeLens()
object CodeLens {
  implicit val clientCapabilitiesTextDocumentCodeLensDecoder
    : Decoder[CodeLens] =
    deriveDecoder
}
