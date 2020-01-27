package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder
import org.enso.gateway.protocol.response.result.servercapabilities.textdocumentsync.TextDocumentSyncKind

/** Defines how text documents are synced.
  *
  * Is either a detailed structure defining each notification or for backwards
  * compatibility the [[TextDocumentSyncKind]] number. If omitted it defaults to
  * [[TextDocumentSyncKind.NoneKind]].
  */
sealed trait TextDocumentSync
object TextDocumentSync {

  case class Number(value: Int) extends TextDocumentSync
  object Number {
    implicit val textDocumentSyncNumberEncoder: Encoder[Number] =
      deriveUnwrappedEncoder
  }

  case class TextDocumentSyncOptions(
    openClose: Option[Boolean]           = None,
    change: Option[TextDocumentSyncKind] = None
  ) extends TextDocumentSync
  object TextDocumentSyncOptions {
    implicit val textDocumentSyncTextDocumentSyncOptionsEncoder
      : Encoder[TextDocumentSyncOptions] = deriveEncoder
  }

  case class WillSaveWaitUntil(willSaveWaitUntil: Boolean)
      extends TextDocumentSync
  object WillSaveWaitUntil {
    implicit val textDocumentSyncWillSaveWaitUntilEncoder
      : Encoder[WillSaveWaitUntil] = deriveEncoder
  }

  implicit val serverCapabilitiesTextDocumentSyncEncoder
    : Encoder[TextDocumentSync] = Encoder.instance {
    case number: Number                      => number.asJson
    case capability: TextDocumentSyncOptions => capability.asJson
    case capability: WillSaveWaitUntil       => capability.asJson
  }
}
