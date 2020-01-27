package org.enso.gateway.protocol.request.clientcapabilities.textdocument.completion

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/** Array of [[CompletionItemKind]]. */
case class CompletionItemKinds(
  valueSet: Option[Seq[CompletionItemKind]] = None
) extends AnyVal
object CompletionItemKinds {
  implicit val clientCapabilitiesTextDocumentCompletionItemKindHolderDecoder
    : Decoder[CompletionItemKinds] =
    deriveDecoder
}
