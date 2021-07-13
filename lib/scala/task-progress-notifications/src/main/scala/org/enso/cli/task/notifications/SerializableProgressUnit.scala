package org.enso.cli.task.notifications

import enumeratum._
import org.enso.cli.task.{TaskProgress, ProgressUnit => TaskProgressUnit}

/** Represents the unit used by progress updates. */
sealed trait SerializableProgressUnit extends EnumEntry
object SerializableProgressUnit
    extends Enum[SerializableProgressUnit]
    with CirceEnum[SerializableProgressUnit] {

  /** Indicates that progress is measured by amount of bytes processed. */
  case object Bytes extends SerializableProgressUnit

  /** Indicates that progress is measured by some other unit or it is not
    * measured at all.
    */
  case object Other extends SerializableProgressUnit

  override val values = findValues

  /** Creates a [[ProgressUnit]] from the unit associated with [[TaskProgress]].
    */
  def fromTask(task: TaskProgress[_]): SerializableProgressUnit =
    task.unit match {
      case TaskProgressUnit.Bytes       => Bytes
      case TaskProgressUnit.Unspecified => Other
    }
}
