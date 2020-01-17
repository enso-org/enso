package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/completion` request. */
case class Completion()
object Completion {
  implicit val clientCapabilitiesTextDocumentCompletionDecoder
    : Decoder[Completion] =
    deriveDecoder
}
