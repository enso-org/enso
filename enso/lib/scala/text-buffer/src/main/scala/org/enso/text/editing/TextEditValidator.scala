package org.enso.text.editing

import cats.implicits._
import org.enso.text.editing.model.TextEdit

/** A validator of [[TextEdit]] object.
  */
object TextEditValidator {

  /** Validates [[TextEdit]] object against the provided buffer.
    *
    * @param buffer a text buffer
    * @param textEdit a change to the buffer
    * @tparam A a buffer type
    * @return
    */
  def validate[A: TextEditor](
    buffer: A,
    textEdit: TextEdit
  ): Either[TextEditValidationFailure, Unit] = {
    for {
      _ <- checkIfEndIsAfterStart(textEdit)
      _ <- checkIfNonNegativeCoords(textEdit.range.start)
      _ <- checkIfNonNegativeCoords(textEdit.range.end)
      _ <- checkIfInsideBuffer(buffer, textEdit.range.start)
      _ <- checkIfInsideBuffer(buffer, textEdit.range.end)
    } yield ()
  }

  private def checkIfEndIsAfterStart(
    textEdit: model.TextEdit
  ): Either[TextEditValidationFailure, Unit] =
    if (textEdit.range.end < textEdit.range.start) {
      Left(EndPositionBeforeStartPosition)
    } else {
      Right(())
    }

  private def checkIfNonNegativeCoords(
    start: model.Position
  ): Either[TextEditValidationFailure, Unit] =
    checkIfNotNegative(start.line) >> checkIfNotNegative(start.character)

  private def checkIfNotNegative(
    coord: Int
  ): Either[TextEditValidationFailure, Unit] =
    if (coord >= 0) Right(())
    else Left(NegativeCoordinateInPosition)

  private def checkIfInsideBuffer[A: TextEditor](
    buffer: A,
    position: model.Position
  ): Either[TextEditValidationFailure, Unit] = {
    val lineCount = TextEditor[A].getLineCount(buffer)
    if (position.line >= lineCount) {
      Left(InvalidPosition(position))
    } else {
      val line = TextEditor[A].getLine(buffer, position.line)
      if (position.character > line.length) {
        Left(InvalidPosition(position))
      } else {
        Right(())
      }
    }
  }

}
