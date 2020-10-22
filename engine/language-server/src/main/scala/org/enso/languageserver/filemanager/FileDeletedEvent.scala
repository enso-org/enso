package org.enso.languageserver.filemanager

import org.enso.languageserver.event.Event

/** A notification about successful file deletion action
  *
  * @param file the deleted file
  */
case class FileDeletedEvent(file: Path) extends Event
