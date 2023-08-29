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
      _ <- checkIfNonNegativeCoords(textEdit.range.start, "text edit start")
      _ <- checkIfNonNegativeCoords(textEdit.range.end, "text edit end")
      _ <- checkIfInsideBuffer(buffer, textEdit.range.start, "text edit start")
      _ <- checkIfInsideBuffer(buffer, textEdit.range.end, "text edit end")
    } yield ()
  }

  private def checkIfEndIsAfterStart(
    textEdit: model.TextEdit
  ): Either[TextEditValidationFailure, Unit] =
    if (textEdit.range.end < textEdit.range.start) {
      Left(
        EndPositionBeforeStartPosition(
          textEdit.range.start,
          textEdit.range.start
        )
      )
    } else {
      Right(())
    }

  private def checkIfNonNegativeCoords(
    start: model.Position,
    reason: => String
  ): Either[TextEditValidationFailure, Unit] =
    checkIfNotNegative(start.line, reason + " line") >> checkIfNotNegative(
      start.character,
      reason + " character"
    )

  private def checkIfNotNegative(
    coord: Int,
    reason: => String
  ): Either[TextEditValidationFailure, Unit] =
    if (coord >= 0) Right(())
    else Left(NegativeCoordinateInPosition(coord, reason))

  private def checkIfInsideBuffer[A: TextEditor](
    buffer: A,
    position: model.Position,
    reason: => String
  ): Either[TextEditValidationFailure, Unit] = {
    val lineCount = TextEditor[A].getLineCount(buffer)
    if (position.line >= lineCount) {
      var extraInfo = ""
      val lastLine  = TextEditor[A].getLine(buffer, lineCount - 1)
      if (lineCount > 0) {
        extraInfo = s", last line '$lastLine`"
      }
      if (lastLine.endsWith(System.lineSeparator())) {
        extraInfo += ", ends with newline"
      }
      Left(
        InvalidPosition(
          position,
          reason + s" line (${position.line}) outside of buffer's line count ($lineCount$extraInfo)"
        )
      )
    } else {
      val line = TextEditor[A].getLine(buffer, position.line)
      if (position.character > line.length) {
        var extraInfo = ""
        val lastLine  = TextEditor[A].getLine(buffer, lineCount - 1)
        if (lineCount > 0) {
          extraInfo = s", last line '$lastLine`"
        }
        if (lastLine.endsWith(System.lineSeparator())) {
          extraInfo += ", ends with newline"
        }
        Left(
          InvalidPosition(
            position,
            s" character (${position.character}) is outside of line's length (${line.length}$extraInfo)"
          )
        )
      } else {
        Right(())
      }
    }
  }

}
