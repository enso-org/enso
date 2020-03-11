package org.enso.languageserver.text.editing

import org.enso.languageserver.filemanager.Path
import org.enso.languageserver.text.Buffer

object model {

  /**
    * A representation of a position in a text file.
    *
    * @param line a line position in a document (zero-based).
    * @param character a character offset
    */
  case class Position(line: Int, character: Int) extends Ordered[Position] {

    override def compare(that: Position): Int = {
      if (this == that) {
        0
      } else if (isThisBefore(that)) {
        -1
      } else {
        1
      }
    }

    private def isThisBefore(that: Position) =
      this.line < that.line || (this.line == that.line && this.character < that.character)

  }

  /**
    * A representation of a range of text in a text file.
    *
    * @param start the range's start position
    * @param end the range's end position
    */
  case class Range(start: Position, end: Position)

  /**
    * A representation of a change to a text file at a given position
    *
    * @param range a range of text in a text file
    * @param text a change to a text file
    */
  case class TextEdit(range: Range, text: String)

  /**
    * A representation of a batch of edits to a file, versioned.
    *
    * @param path a path of a file
    * @param edits a series of edits to a file
    * @param oldVersion the current version of a buffer
    * @param newVersion the version of a buffer after applying all edits
    */
  case class FileEdit(
    path: Path,
    edits: List[TextEdit],
    oldVersion: Buffer.Version,
    newVersion: Buffer.Version
  )

}
