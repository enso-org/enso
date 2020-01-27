package org.enso.gateway.protocol.request.clientcapabilities.textdocument.codeaction

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import org.enso.gateway.protocol.CodeActionKind

/** Array of [[CodeActionKind]]. */
case class CodeActionKinds(valueSet: Seq[CodeActionKind]) extends AnyVal
object CodeActionKinds {
  implicit val codeActionKindsDecoder: Decoder[CodeActionKinds] =
    deriveDecoder
}
