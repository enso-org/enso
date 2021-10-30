package org.enso.text.editing

import org.enso.text.buffer.Rope
import org.enso.text.editing.model.TextEdit

/** TextEditor is a type class that specifies a set of function for text
  * manipulation on a type variable representing a buffer.
  *
  * @tparam A a buffer type
  */
trait TextEditor[A] {

  /** Edits a buffer.
    *
    * @param buffer a text buffer
    * @param diff a change to the buffer
    * @return a modified buffer
    */
  def edit(buffer: A, diff: TextEdit): A

  /** Returns a line count for a buffer.
    *
    * @param buffer a text buffer
    * @return a number of lines in the buffer
    */
  def getLineCount(buffer: A): Int

  /** Cuts out one line from the buffer.
    *
    * @param buffer a text buffer
    * @param lineNumber a number of line
    * @return a line
    */
  def getLine(buffer: A, lineNumber: Int): String

}

object TextEditor {

  def apply[A](implicit textEditor: TextEditor[A]): TextEditor[A] = textEditor

  implicit val ropeTextEditor: TextEditor[Rope] = RopeTextEditor

}
