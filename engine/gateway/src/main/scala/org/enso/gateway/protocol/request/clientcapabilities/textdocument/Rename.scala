package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/rename` request. */
case class Rename(
  dynamicRegistration: Option[Boolean] = None,
  prepareSupport: Option[Boolean]      = None
)
object Rename {
  implicit val clientCapabilitiesTextDocumentRenameDecoder: Decoder[Rename] =
    deriveDecoder
}
