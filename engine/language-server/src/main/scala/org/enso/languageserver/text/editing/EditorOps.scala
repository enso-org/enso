package org.enso.languageserver.text.editing

import cats.implicits._
import org.enso.languageserver.text.editing.TextEditValidator.validate
import org.enso.languageserver.text.editing.model.TextEdit

/**
  * Auxiliary operations that edit a buffer validating diffs before applying it.
  */
object EditorOps {

  /**
    * Type alias for editor computations.
    *
    * @tparam A a result of computations
    */
  type EditorOp[A] = Either[TextEditValidationFailure, A]

  /**
    * Edits a buffer and validates diff.
    *
    * @param buffer a text buffer
    * @param diff a change to the buffer
    * @tparam A a buffer type
    * @return either validation failure or a modified buffer
    */
  def edit[A: TextEditor](buffer: A, diff: TextEdit): EditorOp[A] =
    validate(buffer, diff).map(_ => TextEditor[A].edit(buffer, diff))

  /**
    * Applies a series of edits to the buffer and validates each diff against
    * the buffer.
    *
    * @param buffer a text buffer
    * @param diffs a series of edits to the buffer
    * @tparam A a buffer type
    * @return either validation failure or a modified buffer
    */
  def applyEdits[A: TextEditor](buffer: A, diffs: Seq[TextEdit]): EditorOp[A] =
    diffs.foldLeft[EditorOp[A]](Right(buffer)) {
      case (maybeBuffer, diff) => maybeBuffer >>= (edit(_, diff))
    }

}
