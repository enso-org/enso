package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Capabilities specific to the `textDocument/codeAction` request. */
case class CodeAction()
object CodeAction {
  implicit val clientCapabilitiesTextDocumentCodeActionDecoder
    : Decoder[CodeAction] =
    deriveDecoder
}
