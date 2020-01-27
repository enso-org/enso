package org.enso.gateway.protocol

import io.circe.{Decoder, Encoder}
import io.circe.generic.extras.semiauto.{
  deriveUnwrappedDecoder,
  deriveUnwrappedEncoder
}
import cats.syntax.functor._
import io.circe.syntax._
import org.enso.languageserver

/** Id of [[RequestOrNotification]] or [[Response]]. */
sealed trait Id {
  def toLsModel: languageserver.Id = this match {
    case Id.Number(value) => languageserver.Id.Number(value)
    case Id.Text(value)   => languageserver.Id.Text(value)
  }
}
object Id {
  def fromLsModel(id: languageserver.Id): Id = id match {
    case languageserver.Id.Number(value) => Id.Number(value)
    case languageserver.Id.Text(value)   => Id.Text(value)
  }

  implicit val idEncoder: Encoder[Id] = Encoder.instance {
    case number: Number => number.asJson
    case text: Text     => text.asJson
  }

  implicit val idDecoder: Decoder[Id] = List[Decoder[Id]](
    Decoder[Number].widen,
    Decoder[Text].widen
  ).reduceLeft(_ or _)

  /** A number id. */
  case class Number(value: Int) extends Id
  object Number {
    implicit val idNumberEncoder: Encoder[Number] = deriveUnwrappedEncoder
    implicit val idNumberDecoder: Decoder[Number] = deriveUnwrappedDecoder
  }

  /** A string id. */
  case class Text(value: String) extends Id
  object Text {
    implicit val idStringEncoder: Encoder[Text] = deriveUnwrappedEncoder
    implicit val idStringDecoder: Decoder[Text] = deriveUnwrappedDecoder
  }
}
