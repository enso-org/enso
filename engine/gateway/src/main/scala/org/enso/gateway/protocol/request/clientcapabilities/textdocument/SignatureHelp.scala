package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import org.enso.gateway.protocol.request.clientcapabilities.textdocument.signaturehelp.SignatureInformation

/** Capabilities specific to the `textDocument/signatureHelp` request. */
case class SignatureHelp(
  dynamicRegistration: Option[Boolean]               = None,
  signatureInformation: Option[SignatureInformation] = None,
  contextSupport: Option[Boolean]                    = None
)
object SignatureHelp {
  implicit val clientCapabilitiesTextDocumentSignatureHelpDecoder
    : Decoder[SignatureHelp] =
    deriveDecoder
}
