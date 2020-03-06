package org.enso.languageserver.event

import org.enso.languageserver.filemanager.Path

/**
  * Base trait for all buffer events.
  */
sealed trait BufferEvent extends Event

/**
  * Notifies the Language Server when new file is opened for editing.
  *
  * @param path the path to a file
  */
case class FileOpened(path: Path) extends BufferEvent

/**
  * Notifies the Language Server when a file is closed for editing.
  *
  * @param path the path to a file
  */
case class FileClosed(path: Path) extends BufferEvent
