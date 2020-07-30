package org.enso.launcher.archive

import org.apache.commons.compress.archivers.{ArchiveEntry, ArchiveInputStream}

private[archive] case class ArchiveIterator(
  archiveInputStream: ArchiveInputStream
) extends Iterator[ArchiveEntry] {
  override def hasNext: Boolean = {
    findNext()
    nextEntry.isDefined
  }

  override def next(): ArchiveEntry = {
    findNext()
    nextEntry match {
      case Some(value) =>
        nextEntry = None
        value
      case None =>
        throw new NoSuchElementException("No more entries in the iterator.")
    }
  }

  private var nextEntry: Option[ArchiveEntry] = None
  private var finished: Boolean               = false
  private def findNext(): Unit = {
    if (nextEntry.isEmpty && !finished) {
      val nextCandidate = archiveInputStream.getNextEntry
      if (nextCandidate == null) {
        finished = true
      } else {
        nextEntry = Some(nextCandidate)
      }
    }
  }
}
