package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.execution.model.PendingEdit

import java.io.File

import scala.collection.mutable

final class PendingFileEdits(
  private val pending: mutable.Map[File, Seq[PendingEdit]] = mutable.Map()
) extends PendingEdits {

  /** @inheritdoc */
  override def enqueue(file: File, edits: Seq[PendingEdit]): Unit =
    pending.get(file) match {
      case Some(v) => pending.update(file, v ++ edits)
      case None    => pending.update(file, edits)
    }

  /** @inheritdoc */
  override def dequeue(file: File): Seq[PendingEdit] =
    pending.remove(file).getOrElse(Seq())

  /** @inheritdoc */
  override def files: Seq[File] =
    pending.keys.toSeq
}
