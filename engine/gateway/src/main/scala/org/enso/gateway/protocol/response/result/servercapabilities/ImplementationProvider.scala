package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder
import org.enso.gateway.protocol.response.result.ServerCapabilities.DocumentSelector

/** Server capability to provide "go to implementation" support. */
sealed trait ImplementationProvider
object ImplementationProvider {

  case class Bool(value: Boolean) extends ImplementationProvider
  object Bool {
    implicit val boolEncoder: Encoder[Bool] = deriveUnwrappedEncoder
  }

  case class ImplementationOptions(workDoneProgress: Option[Boolean] = None)
      extends ImplementationProvider
  object ImplementationOptions {
    implicit val implementationOptionsEncoder: Encoder[ImplementationOptions] =
      deriveEncoder
  }

  case class ImplementationRegistrationOptions(
    documentSelector: Option[DocumentSelector] = None,
    workDoneProgress: Option[Boolean]          = None,
    id: Option[String]                         = None
  ) extends ImplementationProvider
  object ImplementationRegistrationOptions {
    implicit val implementationRegistrationOptionsEncoder
      : Encoder[ImplementationRegistrationOptions] =
      deriveEncoder
  }

  implicit val serverCapabilitiesImplementationProviderEncoder
    : Encoder[ImplementationProvider] =
    Encoder.instance {
      case boolean: Bool                              => boolean.asJson
      case options: ImplementationOptions             => options.asJson
      case options: ImplementationRegistrationOptions => options.asJson
    }
}
