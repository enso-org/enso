package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import org.enso.gateway.protocol.request.clientcapabilities.textdocument.publishdiagnostics.TagSupport

/** Capabilities specific to the `textDocument/publishDiagnostics` notification.
  */
case class PublishDiagnostics(
  relatedInformation: Option[Boolean] = None,
  tagSupport: Option[TagSupport]      = None,
  versionSupport: Option[Boolean]     = None
)
object PublishDiagnostics {
  implicit val clientCapabilitiesTextDocumentPublishDiagnosticsDecoder
    : Decoder[PublishDiagnostics] =
    deriveDecoder
}
