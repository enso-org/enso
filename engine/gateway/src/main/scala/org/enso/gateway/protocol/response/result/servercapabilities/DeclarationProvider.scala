package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.semiauto.deriveEncoder
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import org.enso.gateway.protocol.response.result.ServerCapabilities.DocumentSelector

/** Server capability to provide "go to declaration" support. */
sealed trait DeclarationProvider
object DeclarationProvider {

  case class Bool(value: Boolean) extends DeclarationProvider
  object Bool {
    implicit val boolEncoder: Encoder[Bool] =
      deriveUnwrappedEncoder
  }

  case class DeclarationOptions(workDoneProgress: Option[Boolean] = None)
      extends DeclarationProvider
  object DeclarationOptions {
    implicit val declarationOptionsEncoder: Encoder[DeclarationOptions] =
      deriveEncoder
  }

  case class DeclarationRegistrationOptions(
    workDoneProgress: Option[Boolean]          = None,
    documentSelector: Option[DocumentSelector] = None,
    id: Option[String]                         = None
  ) extends DeclarationProvider
  object DeclarationRegistrationOptions {
    implicit val declarationRegistrationOptionsEncoder
      : Encoder[DeclarationRegistrationOptions] =
      deriveEncoder
  }

  implicit val serverCapabilitiesDeclarationProviderEncoder
    : Encoder[DeclarationProvider] =
    Encoder.instance {
      case boolean: Bool                           => boolean.asJson
      case options: DeclarationOptions             => options.asJson
      case options: DeclarationRegistrationOptions => options.asJson
    }
}
