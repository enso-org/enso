package org.enso.editions

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

/** A helper type to handle special parsing logic of edition names.
  *
  * The issue is that if an edition is called `2021.4` and it is written
  * unquoted inside of a YAML file, that is treated as a floating point
  * number, so special care must be taken to correctly parse it.
  */
case class EditionName(name: String) extends AnyVal {

  /** @inheritdoc */
  override def toString: String = name

  /** Returns the name of the file that is associated with the edition name. */
  def toFileName: String = name + EditionName.editionSuffix
}

object EditionName {

  /** A helper method for constructing an [[EditionName]]. */
  def apply(name: String): EditionName = new EditionName(name)

  /** A [[Decoder]] instance for [[EditionName]] that accepts not only strings
    * but also numbers as valid edition names.
    */
  implicit val editionNameDecoder: Decoder[EditionName] = { json =>
    json
      .as[String]
      .fold[Either[DecodingFailure, Any]](
        _ =>
          if (json.value == Json.Null)
            Left(DecodingFailure("edition cannot be empty", Nil))
          else
            json.as[Int].orElse(json.as[Float]),
        Right(_)
      )
      .map(v => EditionName(v.toString))
  }

  /** An [[Encoder]] instance for serializing [[EditionName]].
    *
    * Regardless of the original representation, the edition name is always
    * serialized as string as this is the most portable and precise format for
    * this datatype.
    */
  implicit val encoder: Encoder[EditionName] = { case EditionName(name) =>
    name.asJson
  }

  /** The filename suffix that is used to create a filename corresponding to a
    * named edition.
    */
  val editionSuffix = ".yaml"

  /** Creates an [[EditionName]] from the corresponding filename.
    *
    * Returns None if the filename does not correspond to an edition.
    */
  def fromFilename(filename: String): Option[EditionName] =
    if (filename.endsWith(editionSuffix))
      Some(EditionName(filename.stripSuffix(editionSuffix)))
    else None
}
