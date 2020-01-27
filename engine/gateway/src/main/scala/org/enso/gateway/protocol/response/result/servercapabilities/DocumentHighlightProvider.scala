package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder

/** Server capability to provide document highlight support. */
sealed trait DocumentHighlightProvider
object DocumentHighlightProvider {

  case class Bool(value: Boolean) extends DocumentHighlightProvider
  object Bool {
    implicit val boolEncoder: Encoder[Bool] =
      deriveUnwrappedEncoder
  }

  case class DocumentHighlightOptions(workDoneProgress: Option[Boolean] = None)
      extends DocumentHighlightProvider
  object DocumentHighlightOptions {
    implicit val documentHighlightOptionsEncoder
      : Encoder[DocumentHighlightOptions] =
      deriveEncoder
  }

  implicit val serverCapabilitiesDocumentHighlightProviderEncoder
    : Encoder[DocumentHighlightProvider] =
    Encoder.instance {
      case boolean: Bool                     => boolean.asJson
      case options: DocumentHighlightOptions => options.asJson
    }
}
