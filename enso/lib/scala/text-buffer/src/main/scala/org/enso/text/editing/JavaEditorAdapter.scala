package org.enso.text.editing

import org.enso.text.buffer.Rope
import org.enso.text.editing.EditorOps.EditorOp
import org.enso.text.editing.model.TextEdit

import scala.jdk.CollectionConverters._

/** A convenience class for using the text editor logic from Java code.
  */
object JavaEditorAdapter {
  implicit private val editor: TextEditor[Rope] = RopeTextEditor

  /** Applies a series of edits to a given text.
    *
    * @param rope the initial text.
    * @param edits the edits to apply.
    * @return the result of applying edits, if they pass the validations.
    */
  def applyEdits(
    rope: Rope,
    edits: java.util.List[TextEdit]
  ): EditorOp[Rope] =
    EditorOps.applyEdits(rope, edits.asScala.toList)

}
