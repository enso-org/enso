package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder
import org.enso.gateway.protocol.response.result.ServerCapabilities.DocumentSelector

/** Server capability to provide "go to type definition" support. */
sealed trait TypeDefinitionProvider
object TypeDefinitionProvider {

  case class Bool(value: Boolean) extends TypeDefinitionProvider
  object Bool {
    implicit val boolEncoder: Encoder[Bool] = deriveUnwrappedEncoder
  }

  case class TypeDefinitionOptions(workDoneProgress: Option[Boolean] = None)
      extends TypeDefinitionProvider
  object TypeDefinitionOptions {
    implicit val typeDefinitionOptionsEncoder: Encoder[TypeDefinitionOptions] =
      deriveEncoder
  }

  case class TypeDefinitionRegistrationOptions(
    documentSelector: Option[DocumentSelector] = None,
    workDoneProgress: Option[Boolean]          = None,
    id: Option[String]                         = None
  ) extends TypeDefinitionProvider

  object TypeDefinitionRegistrationOptions {
    implicit val typeDefinitionRegistrationOptionsEncoder
      : Encoder[TypeDefinitionRegistrationOptions] =
      deriveEncoder
  }

  implicit val serverCapabilitiesTypeDefinitionProviderEncoder
    : Encoder[TypeDefinitionProvider] =
    Encoder.instance {
      case boolean: Bool                              => boolean.asJson
      case options: TypeDefinitionOptions             => options.asJson
      case options: TypeDefinitionRegistrationOptions => options.asJson
    }
}
