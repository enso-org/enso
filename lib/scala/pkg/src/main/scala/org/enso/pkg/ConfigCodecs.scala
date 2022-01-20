package org.enso.pkg

import io.circe._

/** A collection of utility codecs used in the [[Config]]. */
object ConfigCodecs {

  object Fields {
    val Name = "name"
  }

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

  /** Get the name of decoded entity in case when the decoded entity is a JSON
    * object.
    *
    * @param entity the name of decoded entity
    * @param cursor the current focus in the JSON document
    */
  private def getNameKey(
    entity: String,
    cursor: HCursor
  ): Decoder.Result[String] =
    cursor.keys match {
      case Some(keys) if keys.nonEmpty =>
        cursor.get[Option[String]](keys.head).flatMap {
          case Some(_) =>
            Left(decodingFailure(s"$entity name", cursor.history))
          case None =>
            Right(keys.head)
        }
      case _ =>
        Left(decodingFailure(s"$entity name", cursor.history))
    }

  /** Get the name of decoded entity in case when the decoded entity is a JSON
    * string.
    *
    * @param entity the decoded entity
    * @param cursor the current focus in the JSON document
    */
  private def getNameString(
    entity: String,
    cursor: HCursor
  ): Decoder.Result[String] =
    cursor
      .as[String]
      .left
      .map(f => decodingFailure(s"$entity name", f.history))

  private def getNameField(cursor: HCursor): Decoder.Result[String] =
    cursor.get[String](Fields.Name)

  /** Get the name of decoded entity.
    *
    * @param entity the decoded entity
    * @param cursor the current focus in the JSON document
    */
  def getName(entity: String)(cursor: HCursor): Decoder.Result[String] =
    getNameField(cursor)
      .orElse(getNameKey(entity, cursor))
      .orElse(getNameString(entity, cursor))

  /** Get the scalar value of the provided JSON element.
    *
    * @param entity the name of decoded entity
    * @param cursor the current focus in the JSON document
    */
  def getScalar(entity: String)(cursor: HCursor): Decoder.Result[String] =
    cursor.value.fold(
      jsonNull    = Left(decodingFailure(entity, cursor.history)),
      jsonBoolean = value => Right(value.toString),
      jsonNumber  = value => Right(value.toString),
      jsonString  = value => Right(value),
      jsonArray   = _ => Left(decodingFailure(entity, cursor.history)),
      jsonObject  = _ => Left(decodingFailure(entity, cursor.history))
    )

  /** Get the set of JSON object keys.
    *
    * @param cursor the current focus in the JSON document
    */
  def objectKeys(cursor: HCursor): Set[String] =
    cursor.keys.getOrElse(Seq()).toSet
}
