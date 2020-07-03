package org.enso.text.editing

import org.enso.text.editing.model.Position

/**
  * Base trait for text edit validation failures.
  */
sealed trait TextEditValidationFailure

/**
  * Signals that an end position is before a start position.
  */
case object EndPositionBeforeStartPosition extends TextEditValidationFailure

/**
  * Signals the a position object contains negative coordinates.
  */
case object NegativeCoordinateInPosition extends TextEditValidationFailure

/**
  * Signals that a position is outside the buffer.
  *
  * @param position an invalid position
  */
case class InvalidPosition(position: Position) extends TextEditValidationFailure
