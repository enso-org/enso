package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/definition` request. */
case class Definition()
object Definition {
  implicit val clientCapabilitiesTextDocumentDefinitionDecoder
    : Decoder[Definition] =
    deriveDecoder
}
