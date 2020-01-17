package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

/** Defines how text documents are synced. Is either a detailed structure
  * defining each notification or for backwards compatibility the
  * `TextDocumentSyncKind` number. If omitted it defaults to
  * `TextDocumentSyncKind.None`.
  */
case class TextDocumentSync()
object TextDocumentSync {
  implicit val serverCapabilitiesTextDocumentSyncEncoder
    : Encoder[TextDocumentSync] =
    deriveEncoder
}
