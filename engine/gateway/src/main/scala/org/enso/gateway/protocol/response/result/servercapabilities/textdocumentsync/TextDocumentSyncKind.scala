package org.enso.gateway.protocol.response.result.servercapabilities.textdocumentsync

import io.circe.Encoder

/** Kind of document sync. */
sealed abstract class TextDocumentSyncKind(val value: Int)
object TextDocumentSyncKind {
  private val none        = 0
  private val full        = 1
  private val incremental = 2

  /** Signals that documents should not be synced at all. */
  object NoneKind extends TextDocumentSyncKind(none)

  /** Signals that documents are synced by always sending the full content of
    * the document.
    */
  object Full extends TextDocumentSyncKind(full)

  /** Signals that documents are synced by sending the full content on open.
    *
    * After that only incremental updates to the document are sent.
    */
  object Incremental extends TextDocumentSyncKind(incremental)

  implicit val textDocumentSyncKindEncoder: Encoder[TextDocumentSyncKind] =
    Encoder.encodeInt.contramap(_.value)
}
