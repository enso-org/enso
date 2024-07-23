package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.execution.model.PendingEdit
import org.enso.text.editing.model.IdMap

import java.io.File

import scala.collection.mutable

final class PendingFileEdits(
  private val pending: mutable.Map[File, Seq[PendingEdit]] = mutable.Map(),
  private val idMaps: mutable.Map[File, IdMap]             = mutable.Map()
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
  override def putIdMap(file: File, idMap: IdMap): Unit =
    idMaps.updateWith(file) {
      case Some(v) => Some(IdMap(v.values :++ idMap.values))
      case None    => Some(idMap)
    }

  /** @inheritdoc */
  override def takeIdMap(file: File): Option[IdMap] =
    idMaps.remove(file)

  /** @inheritdoc */
  override def files: Seq[File] =
    (pending.keySet ++ idMaps.keySet).toSeq
}
