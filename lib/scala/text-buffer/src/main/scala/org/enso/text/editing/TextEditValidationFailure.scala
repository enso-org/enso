package org.enso.text.editing

import org.enso.text.editing.model.Position

/** Base trait for text edit validation failures.
  */
sealed trait TextEditValidationFailure

/** Signals that an end position is before a start position.
  */
case class EndPositionBeforeStartPosition(start: Position, end: Position)
    extends TextEditValidationFailure

/** Signals the a position object contains negative coordinates.
  */
case class NegativeCoordinateInPosition(coordinates: Int, checkReason: String)
    extends TextEditValidationFailure

/** Signals that a position is outside the buffer.
  *
  * @param position an invalid position
  */
case class InvalidPosition(position: Position, checkReason: String)
    extends TextEditValidationFailure
