package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder
import org.enso.gateway.protocol.response.result.ServerCapabilities.DocumentSelector

/** Server capability to provide folding provider support. */
sealed trait FoldingRangeProvider
object FoldingRangeProvider {

  case class Bool(value: Boolean) extends FoldingRangeProvider
  object Bool {
    implicit val boolEncoder: Encoder[Bool] = deriveUnwrappedEncoder
  }

  case class FoldingRangeOptions(workDoneProgress: Option[Boolean] = None)
      extends FoldingRangeProvider
  object FoldingRangeOptions {
    implicit val foldingRangeOptionsEncoder: Encoder[FoldingRangeOptions] =
      deriveEncoder
  }

  case class FoldingRangeRegistrationOptions(
    documentSelector: Option[DocumentSelector] = None,
    workDoneProgress: Option[Boolean]          = None,
    id: Option[String]                         = None
  ) extends FoldingRangeProvider
  object FoldingRangeRegistrationOptions {
    implicit val foldingRangeRegistrationOptionsEncoder
      : Encoder[FoldingRangeRegistrationOptions] =
      deriveEncoder
  }

  implicit val serverCapabilitiesFoldingRangeProviderEncoder
    : Encoder[FoldingRangeProvider] =
    Encoder.instance {
      case boolean: Bool                            => boolean.asJson
      case options: FoldingRangeOptions             => options.asJson
      case options: FoldingRangeRegistrationOptions => options.asJson
    }
}
