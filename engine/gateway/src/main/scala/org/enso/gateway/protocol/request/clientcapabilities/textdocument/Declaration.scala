package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/declaration` request. */
case class Declaration()
object Declaration {
  implicit val clientCapabilitiesTextDocumentDeclarationDecoder
    : Decoder[Declaration] =
    deriveDecoder
}
