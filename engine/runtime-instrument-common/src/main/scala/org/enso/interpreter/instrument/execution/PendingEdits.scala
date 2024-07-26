package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.execution.model.PendingEdit
import org.enso.text.editing.model.IdMap

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

  /** Update the IdMap of the file.
    *
    * @param file the edited file
    * @param idMap the new updated IdMap
    */
  def updateIdMap(file: File, idMap: IdMap): Unit

  /** Consume the idMap of the file.
    *
    * @param file the edited file
    * @return the IdMap of the file
    */
  def removeIdMap(file: File): Option[IdMap]

  /** List files with pending edits.
    *
    * @return the list of files with pending edits
    */
  def files: Seq[File]
}
