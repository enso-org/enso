package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder

/** Server capability to provide document range formatting. */
sealed trait DocumentRangeFormattingProvider
object DocumentRangeFormattingProvider {

  case class Bool(value: Boolean) extends DocumentRangeFormattingProvider
  object Bool {
    implicit val boolEncoder: Encoder[Bool] =
      deriveUnwrappedEncoder
  }

  case class DocumentRangeFormattingOptions(
    workDoneProgress: Option[Boolean] = None
  ) extends DocumentRangeFormattingProvider
  object DocumentRangeFormattingOptions {
    implicit val DocumentRangeFormattingOptiondEncoder
      : Encoder[DocumentRangeFormattingOptions] =
      deriveEncoder
  }

  implicit val serverCapabilitiesDocumentRangeFormattingProviderEncoder
    : Encoder[DocumentRangeFormattingProvider] =
    Encoder.instance {
      case boolean: Bool                           => boolean.asJson
      case options: DocumentRangeFormattingOptions => options.asJson
    }
}
