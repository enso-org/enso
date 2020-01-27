package org.enso.gateway.protocol.response

import io.circe.{Encoder, Json}
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder
import org.enso.gateway.protocol.response.result.{
  ServerCapabilities,
  ServerInfo
}
import io.circe.syntax._

/** Result of [[org.enso.gateway.protocol.Response]].
  *
  * LSP Spec:
  * https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#responseMessage
  */
sealed trait Result
object Result {
  implicit val resultEncoder: Encoder[Result] = Encoder.instance {
    case text: Text               => text.asJson
    case number: Number           => number.asJson
    case boolean: Bool            => boolean.asJson
    case result: InitializeResult => result.asJson
    case result: NullResult.type  => result.asJson
  }

  /** A string result. */
  case class Text(value: String) extends Result
  object Text {
    implicit val resultStringEncoder: Encoder[Text] = deriveUnwrappedEncoder
  }

  /** A number result. */
  case class Number(value: Int) extends Result
  object Number {
    implicit val resultNumberEncoder: Encoder[Number] = deriveUnwrappedEncoder
  }

  /** A boolean result. */
  case class Bool(value: Boolean) extends Result
  object Bool {
    implicit val resultBooleanEncoder: Encoder[Bool] =
      deriveUnwrappedEncoder
  }

  /** Result of [[org.enso.gateway.protocol.Requests.Initialize]]. */
  case class InitializeResult(
    capabilities: ServerCapabilities,
    serverInfo: Option[ServerInfo] = None
  ) extends Result
  object InitializeResult {
    implicit val initializeResultEncoder: Encoder[InitializeResult] =
      deriveEncoder
  }

  /** Result of [[org.enso.gateway.protocol.Requests.Shutdown]]. */
  case object NullResult extends Result {
    implicit val nullResultEncoder: Encoder[NullResult.type] = _ => Json.Null
  }
}
