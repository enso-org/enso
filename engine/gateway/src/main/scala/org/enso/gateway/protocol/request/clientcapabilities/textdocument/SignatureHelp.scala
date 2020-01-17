package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/signatureHelp` request. */
case class SignatureHelp()
object SignatureHelp {
  implicit val clientCapabilitiesTextDocumentSignatureHelpDecoder
    : Decoder[SignatureHelp] =
    deriveDecoder
}
