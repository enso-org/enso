package org.enso.pkg

import io.circe._
import io.circe.syntax._

/** A collection of utility codecs used in the [[Config]]. */
object ConfigCodecs {

  /** The common decoding failure.
    *
    * @param entity the name of decoded entity
    * @param history the list of JSON cursor operations
    */
  private def decodingFailure(
    entity: String,
    history: List[CursorOp]
  ): DecodingFailure =
    DecodingFailure(s"Failed to decode $entity", history)

  /** Try to decode the entity `A` from a JSON object.
    *
    * The entity can be encoded either in the first key of the JSON object with
    * the Null value,
    * {{{
    *   { entity: null }
    * }}}
    *
    * or as a value of the provided `keyName` argument
    *
    * {{{
    *   { `keyName`: entity }
    * }}}
    *
    * @param entity the name of decoded entity that is used in error reporting
    * @param keyName the key of the JSON object that contains the entity
    * @param cursor the current focus in the JSON document
    */
  def getFromObject[A: Decoder](
    entity: String,
    keyName: String,
    cursor: HCursor
  ): Decoder.Result[A] =
    cursor.keys match {
      case Some(keys) if keys.nonEmpty =>
        cursor
          .get[A](keyName)
          .orElse {
            cursor.get[Json](keys.head).flatMap { json =>
              if (json.isNull) {
                Decoder[A].decodeJson(keys.head.asJson)
              } else {
                Left(decodingFailure(entity, cursor.history))
              }
            }
          }
      case _ =>
        Left(decodingFailure(entity, cursor.history))
    }

  /** Get the scalar value of the provided JSON element.
    *
    * @param entity the name of decoded entity
    * @param cursor the current focus in the JSON document
    */
  def getScalar(entity: String, cursor: HCursor): Decoder.Result[String] =
    cursor.value.fold(
      jsonNull    = Left(decodingFailure(entity, cursor.history)),
      jsonBoolean = value => Right(value.toString),
      jsonNumber  = value => Right(value.toString),
      jsonString  = value => Right(value),
      jsonArray   = _ => Left(decodingFailure(entity, cursor.history)),
      jsonObject  = _ => Left(decodingFailure(entity, cursor.history))
    )
}
