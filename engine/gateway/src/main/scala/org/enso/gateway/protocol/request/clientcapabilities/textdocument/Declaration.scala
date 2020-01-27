package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/declaration` request. */
case class Declaration(
  dynamicRegistration: Option[Boolean] = None,
  linkSupport: Option[Boolean]         = None
)
object Declaration {
  implicit val clientCapabilitiesTextDocumentDeclarationDecoder
    : Decoder[Declaration] =
    deriveDecoder
}
