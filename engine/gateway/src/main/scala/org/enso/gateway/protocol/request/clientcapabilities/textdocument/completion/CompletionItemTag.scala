package org.enso.gateway.protocol.request.clientcapabilities.textdocument.completion

import io.circe.Decoder

/** Tag of [[CompletionItem]]. */
sealed abstract class CompletionItemTag(value: Int)
object CompletionItemTag {
  private val deprecated               = 1
  private val invalidCompletionItemTag = "Invalid CompletionItemTag"

  object Deprecated extends CompletionItemTag(deprecated)

  implicit val textDocumentCompletionItemTagDecoder
    : Decoder[CompletionItemTag] =
    Decoder.decodeInt.emap {
      case `deprecated` => Right(Deprecated)
      case _            => Left(invalidCompletionItemTag)
    }
}
