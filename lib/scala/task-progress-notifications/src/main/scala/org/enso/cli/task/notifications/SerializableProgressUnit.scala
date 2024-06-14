package org.enso.cli.task.notifications

import io.circe.{Decoder, Encoder}
import org.enso.cli.task.{ProgressUnit => TaskProgressUnit}

/** Represents the unit used by progress updates. */
object SerializableProgressUnits extends Enumeration {
  type SerializableProgressUnit = Value

  /** Byte indicates that progress is measured by amount of bytes processed.
    * Other indicates that progress is measured by some other unit or it is not
    * measured at all.
    */
  val Bytes, Other = Value

  /** Converts a [[TaskProgressUnit]] to [[SerializableProgressUnit]].
    */
  implicit def fromUnit(unit: TaskProgressUnit): SerializableProgressUnit =
    unit match {
      case TaskProgressUnit.Bytes       => Bytes
      case TaskProgressUnit.Unspecified => Other
    }

  implicit val genderDecoder: Decoder[SerializableProgressUnit] =
    Decoder.decodeEnumeration(SerializableProgressUnits)
  implicit val genderEncoder: Encoder[SerializableProgressUnit] =
    Encoder.encodeEnumeration(SerializableProgressUnits)
}
