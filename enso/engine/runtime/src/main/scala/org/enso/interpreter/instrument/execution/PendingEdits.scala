package org.enso.interpreter.instrument.execution

import java.io.File

import org.enso.text.editing.model.TextEdit

trait PendingEdits {

  /** Enqueue pending file edits.
    *
    * @param file the edited file
    * @param edits the list of file edits
    */
  def enqueue(file: File, edits: Seq[TextEdit]): Unit

  /** Dequeue pending file edits.
    *
    * @param file the edited file
    * @return the list of pending edits
    */
  def dequeue(file: File): Seq[TextEdit]
}
