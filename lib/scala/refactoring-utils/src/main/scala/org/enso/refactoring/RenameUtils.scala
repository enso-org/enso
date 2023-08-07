package org.enso.refactoring

import org.enso.syntax.text.Location
import org.enso.text.editing.{model, IndexedSource}
import org.enso.text.editing.model.TextEdit

object RenameUtils {

  /** Create a list of edits that should be made to rename the provided
    * occurrences of text.
    *
    * @param source the original source
    * @param occurrences the occurrences in the source that should be replaced
    * @param newText the text to replace in the provided locations
    * @return a list of text edits that should be applied to the original source
    * in order to replace the provided text occurrences with the new text.
    */
  def buildEdits[A: IndexedSource](
    source: A,
    occurrences: Seq[Location],
    newText: String
  ): Seq[TextEdit] = {
    val (_, builder) = occurrences
      .sortBy(_.start)
      .foldLeft((0, Vector.newBuilder[TextEdit])) {
        case ((offset, builder), location) =>
          val start =
            implicitly[IndexedSource[A]]
              .toPosition(location.start + offset, source)
          val end =
            implicitly[IndexedSource[A]]
              .toPosition(location.end + offset, source)
          val range = model.Range(start, end)

          val newOffset = offset - location.length + newText.length
          val textEdit  = TextEdit(range, newText)
          (newOffset, builder += textEdit)
      }

    builder.result()
  }
}
