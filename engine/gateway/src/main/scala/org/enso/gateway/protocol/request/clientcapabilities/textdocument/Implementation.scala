package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/implementation` request. */
case class Implementation(
  dynamicRegistration: Option[Boolean] = None,
  linkSupport: Option[Boolean]         = None
)
object Implementation {
  implicit val clientCapabilitiesTextDocumentImplementationDecoder
    : Decoder[Implementation] =
    deriveDecoder
}
