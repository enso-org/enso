package org.enso.text.editing

import org.enso.text.buffer.Rope
import org.enso.text.editing.model.TextEdit

/**
  * Instance of the [[TextEditor]] type class for the [[Rope]] type.
  */
object RopeTextEditor extends TextEditor[Rope] {

  /**
    * Edits a buffer.
    *
    * @param buffer a text buffer
    * @param diff a change to the buffer
    * @return a modified buffer
    */
  override def edit(buffer: Rope, diff: TextEdit): Rope = {
    val head: Rope = cutOutHead(buffer, diff)
    val tail: Rope = cutOutTail(buffer, diff)

    head ++ Rope(diff.text) ++ tail
  }

  private def cutOutHead(buffer: Rope, diff: TextEdit): Rope = {
    val fullLines = buffer.lines.take(diff.range.start.line)

    val rest =
      if (diff.range.start.character > 0)
        buffer.lines
          .drop(diff.range.start.line)
          .codePoints
          .take(diff.range.start.character)
      else
        Rope.empty

    fullLines ++ rest
  }

  private def cutOutTail(buffer: Rope, diff: TextEdit): Rope =
    buffer.lines
      .drop(diff.range.end.line)
      .codePoints
      .drop(diff.range.end.character)

  /**
    * Returns a number of lines in a buffer.
    *
    * @param buffer a text buffer
    * @return a number of lines in the buffer
    */
  override def getLineCount(buffer: Rope): Int = buffer.lines.length

  /**
    * Cuts out one line from the buffer.
    *
    * @param buffer a text buffer
    * @param lineNumber a number of line
    * @return a line
    */
  override def getLine(buffer: Rope, lineNumber: Int): String = {
    val tail = buffer.lines.drop(lineNumber)
    val line = tail.lines.take(1)
    line.toString
  }
}
