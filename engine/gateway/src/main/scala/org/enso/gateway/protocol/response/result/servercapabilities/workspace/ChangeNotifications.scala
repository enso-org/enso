package org.enso.gateway.protocol.response.result.servercapabilities.workspace

import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder

/** Part of [[WorkspaceFoldersServerCapabilities]]. */
sealed trait ChangeNotifications
object ChangeNotifications {

  /** String [[ChangeNotifications]]. */
  case class Text(value: String) extends ChangeNotifications
  object Text {
    implicit val textEncoder: Encoder[Text] =
      deriveUnwrappedEncoder
  }

  /** Boolean [[ChangeNotifications]]. */
  case class Bool(value: Boolean) extends ChangeNotifications
  object Bool {
    implicit val boolEncoder: Encoder[Bool] =
      deriveUnwrappedEncoder
  }

  implicit val textEncoder: Encoder[ChangeNotifications] =
    Encoder.instance {
      case text: Text    => text.asJson
      case boolean: Bool => boolean.asJson
    }
}
