package org.enso.gateway.protocol.request.clientcapabilities.textdocument

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import org.enso.gateway.protocol.request.clientcapabilities.textdocument.completion.{
  CompletionItem,
  CompletionItemKinds
}

/** Capabilities specific to the `textDocument/completion` request. */
case class Completion(
  dynamicRegistration: Option[Boolean]            = None,
  completionItem: Option[CompletionItem]          = None,
  completionItemKind: Option[CompletionItemKinds] = None,
  contextSupport: Option[Boolean]                 = None
)
object Completion {
  implicit val clientCapabilitiesTextDocumentCompletionDecoder
    : Decoder[Completion] =
    deriveDecoder
}
