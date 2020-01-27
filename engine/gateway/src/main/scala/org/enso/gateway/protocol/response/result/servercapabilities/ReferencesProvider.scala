package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder

/** Server capability to provide find references support. */
sealed trait ReferencesProvider
object ReferencesProvider {

  case class Bool(value: Boolean) extends ReferencesProvider
  object Bool {
    implicit val boolEncoder: Encoder[Bool] = deriveUnwrappedEncoder
  }

  case class ReferenceOptions(workDoneProgress: Option[Boolean] = None)
      extends ReferencesProvider
  object ReferenceOptions {
    implicit val referenceOptionsEncoder: Encoder[ReferenceOptions] =
      deriveEncoder
  }

  implicit val serverCapabilitiesReferencesProviderEncoder
    : Encoder[ReferencesProvider] =
    Encoder.instance {
      case boolean: Bool             => boolean.asJson
      case options: ReferenceOptions => options.asJson
    }
}
