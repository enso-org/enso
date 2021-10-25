package org.enso.interpreter.instrument.execution

import java.io.File

import org.enso.text.editing.model.TextEdit

import scala.collection.mutable

final class PendingFileEdits(
  private val pending: mutable.Map[File, Seq[TextEdit]] = mutable.Map()
) extends PendingEdits {

  /** @inheritdoc */
  override def enqueue(file: File, edits: Seq[TextEdit]): Unit =
    pending.get(file) match {
      case Some(v) => pending.update(file, v ++ edits)
      case None    => pending.update(file, edits)
    }

  /** @inheritdoc */
  override def dequeue(file: File): Seq[TextEdit] =
    pending.remove(file).getOrElse(Seq())
}
