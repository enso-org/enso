package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.execution.model.PendingEdit

import java.io.File

trait PendingEdits {

  /** Enqueue pending file edits.
    *
    * @param file the edited file
    * @param edits the list of file edits
    */
  def enqueue(file: File, edits: Seq[PendingEdit]): Unit

  /** Dequeue pending file edits.
    *
    * @param file the edited file
    * @return the list of pending edits
    */
  def dequeue(file: File): Seq[PendingEdit]

  /** List files with pending edits.
    *
    * @return the list of files with pending edits
    */
  def files: Seq[File]
}
