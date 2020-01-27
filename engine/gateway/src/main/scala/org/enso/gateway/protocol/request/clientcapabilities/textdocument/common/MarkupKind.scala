package org.enso.gateway.protocol.request.clientcapabilities.textdocument.common

import io.circe.Decoder
import io.circe.generic.extras.semiauto.deriveEnumerationDecoder

/** Part of
  * [[org.enso.gateway.protocol.request.clientcapabilities.textdocument.completion.CompletionItem]]
  * and
  * [[org.enso.gateway.protocol.request.clientcapabilities.textdocument.signaturehelp.SignatureInformation]].
  */
sealed trait MarkupKind
object MarkupKind {

  /** Plain text is supported as a content format. */
  case object plaintext extends MarkupKind

  /** Markdown is supported as a content format. */
  case object markdown extends MarkupKind

  implicit val clientCapabilitiesTextDocumentCompletionMarkupKindDecoder
    : Decoder[MarkupKind] = deriveEnumerationDecoder
}
