package org.enso.gateway.protocol.request.clientcapabilities.textdocument.completion

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import org.enso.gateway.protocol.request.clientcapabilities.textdocument.common.MarkupKind

/** Part of
  * [[org.enso.gateway.protocol.request.clientcapabilities.textdocument.Completion]].
  */
case class CompletionItem(
  snippetSupport: Option[Boolean]              = None,
  commitCharactersSupport: Option[Boolean]     = None,
  documentationFormat: Option[Seq[MarkupKind]] = None,
  deprecatedSupport: Option[Boolean]           = None,
  preselectSupport: Option[Boolean]            = None,
  tagSupport: Option[TagSupport]               = None
)
object CompletionItem {
  implicit val clientCapabilitiesTextDocumentCompletionItemDecoder
    : Decoder[CompletionItem] =
    deriveDecoder
}
