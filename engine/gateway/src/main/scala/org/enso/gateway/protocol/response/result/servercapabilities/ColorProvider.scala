package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder
import org.enso.gateway.protocol.response.result.ServerCapabilities.DocumentSelector

/** Server capability to provide color provider support. */
sealed trait ColorProvider
object ColorProvider {

  case class Bool(value: Boolean) extends ColorProvider
  object Bool {
    implicit val boolEncoder: Encoder[Bool] =
      deriveUnwrappedEncoder
  }

  case class DocumentColorOptions(workDoneProgress: Option[Boolean] = None)
      extends ColorProvider
  object DocumentColorOptions {
    implicit val documentColorOptionsEncoder: Encoder[DocumentColorOptions] =
      deriveEncoder
  }

  case class DocumentColorRegistrationOptions(
    workDoneProgress: Option[Boolean]          = None,
    documentSelector: Option[DocumentSelector] = None,
    id: Option[String]                         = None
  ) extends ColorProvider
  object DocumentColorRegistrationOptions {
    implicit val documentColorRegistrationOptionsEncoder
      : Encoder[DocumentColorRegistrationOptions] =
      deriveEncoder
  }

  implicit val serverCapabilitiesColorProviderEncoder: Encoder[ColorProvider] =
    Encoder.instance {
      case boolean: Bool                             => boolean.asJson
      case options: DocumentColorOptions             => options.asJson
      case options: DocumentColorRegistrationOptions => options.asJson
    }
}
