package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/publishDiagnostics` notification.
  */
case class PublishDiagnostics()
object PublishDiagnostics {
  implicit val clientCapabilitiesTextDocumentPublishDiagnosticsDecoder
    : Decoder[PublishDiagnostics] =
    deriveDecoder
}
