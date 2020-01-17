package org.enso.gateway.protocol.response.error

import io.circe.Encoder
import io.circe.generic.extras.semiauto.deriveUnwrappedEncoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._

/** Data of [[org.enso.gateway.protocol.response.ResponseError]]. */
sealed trait Data
object Data {
  implicit val dataEncoder: Encoder[Data] = Encoder.instance {
    case text: Text                     => text.asJson
    case number: Number                 => number.asJson
    case boolean: Bool                  => boolean.asJson
    case array: Array                   => array.asJson
    case parseData: ParseData           => parseData.asJson
    case initializeData: InitializeData => initializeData.asJson
  }

  /** A string data. */
  case class Text(value: String) extends Data
  object Text {
    implicit val dataStringEncoder: Encoder[Text] = deriveUnwrappedEncoder
  }

  /** A number data. */
  case class Number(value: Int) extends Data
  object Number {
    implicit val dataNumberEncoder: Encoder[Number] = deriveUnwrappedEncoder
  }

  /** A boolean data. */
  case class Bool(value: Boolean) extends Data
  object Bool {
    implicit val dataBooleanEncoder: Encoder[Bool] = deriveUnwrappedEncoder
  }

  /** An array data. */
  case class Array(value: Seq[Option[Datum]]) extends Data
  object Array {
    implicit val dataArrayEncoder: Encoder[Array] = deriveUnwrappedEncoder
  }

  /** Data of [[org.enso.gateway.protocol.response.ResponseError.ParseError]].
    */
  case class ParseData(
    json: String,
    circeMessage: String
  ) extends Data
  object ParseData {
    implicit val parseDataEncoder: Encoder[ParseData] = deriveEncoder
  }

  /** Data of [[org.enso.gateway.protocol.Requests.Initialize]]. */
  case class InitializeData(
    retry: Boolean
  ) extends Data
  object InitializeData {
    implicit val initializeDataEncoder: Encoder[InitializeData] = deriveEncoder
  }
}
