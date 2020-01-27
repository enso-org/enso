package org.enso.gateway.protocol.response.result.servercapabilities

import io.circe.Encoder
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._

/** Server capability to provide hover support. */
sealed trait HoverProvider
object HoverProvider {

  case class Bool(value: Boolean) extends HoverProvider
  object Bool {
    implicit val boolEncoder: Encoder[Bool] = deriveUnwrappedEncoder
  }

  case class HoverOptions(workDoneProgress: Option[Boolean] = None)
      extends HoverProvider
  object HoverOptions {
    implicit val hoverOptionsEncoder: Encoder[HoverOptions] = deriveEncoder
  }

  implicit val serverCapabilitiesHoverProviderEncoder: Encoder[HoverProvider] =
    Encoder.instance {
      case boolean: Bool         => boolean.asJson
      case options: HoverOptions => options.asJson
    }
}
