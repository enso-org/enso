package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder

/** Server capability to provide "go to definition" support. */
sealed trait DefinitionProvider
object DefinitionProvider {

  case class Bool(value: Boolean) extends DefinitionProvider
  object Bool {
    implicit val boolEncoder: Encoder[Bool] =
      deriveUnwrappedEncoder
  }

  case class DefinitionOptions(workDoneProgress: Option[Boolean] = None)
      extends DefinitionProvider
  object DefinitionOptions {
    implicit val definitionOptionsEncoder: Encoder[DefinitionOptions] =
      deriveEncoder
  }

  implicit val serverCapabilitiesDefinitionProviderEncoder
    : Encoder[DefinitionProvider] =
    Encoder.instance {
      case boolean: Bool              => boolean.asJson
      case options: DefinitionOptions => options.asJson
    }
}
