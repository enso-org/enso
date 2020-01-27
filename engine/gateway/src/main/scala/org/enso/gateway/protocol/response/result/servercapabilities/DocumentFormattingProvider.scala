package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder

/** Server capability to provide document formatting. */
sealed trait DocumentFormattingProvider
object DocumentFormattingProvider {

  case class Bool(value: Boolean) extends DocumentFormattingProvider
  object Bool {
    implicit val boolEncoder: Encoder[Bool] =
      deriveUnwrappedEncoder
  }

  case class DocumentFormattingOptions(workDoneProgress: Option[Boolean] = None)
      extends DocumentFormattingProvider
  object DocumentFormattingOptions {
    implicit val documentFormattingOptionsEncoder
      : Encoder[DocumentFormattingOptions] =
      deriveEncoder
  }

  implicit val serverCapabilitiesDocumentFormattingProviderEncoder
    : Encoder[DocumentFormattingProvider] =
    Encoder.instance {
      case boolean: Bool                      => boolean.asJson
      case options: DocumentFormattingOptions => options.asJson
    }
}
