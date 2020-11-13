package org.enso.languageserver.filemanager

import java.io.File

import enumeratum._

/**
  * A representation of filesystem event.
  *
  * @param path path to the file system object
  * @param kind type of file system event
  */
case class FileEvent(path: Path, kind: FileEventKind)

object FileEvent {

  /**
    * Conversion from file system event.
    *
    * @param root a project root
    * @param base a watched path
    * @param event a file system event
    * @return file event
    */
  def fromWatcherEvent(
    root: File,
    base: Path,
    event: WatcherAdapter.WatcherEvent
  ): FileEvent =
    FileEvent(
      Path.getRelativePath(root, base, event.path),
      FileEventKind(event.eventType)
    )
}

/**
  * Type of a file event.
  */
sealed trait FileEventKind extends EnumEntry

object FileEventKind extends Enum[FileEventKind] with CirceEnum[FileEventKind] {

  /**
    * Event type indicating file creation.
    */
  case object Added extends FileEventKind

  /**
    * Event type indicating file deletion.
    */
  case object Removed extends FileEventKind

  /**
    * Event type indicating file modification.
    */
  case object Modified extends FileEventKind

  override val values = findValues

  /**
    * Create [[FileEventKind]] from [[WatcherAdapter.EventType]].
    *
    * @param eventType file system event type
    * @return file event kind
    */
  def apply(eventType: WatcherAdapter.EventType): FileEventKind =
    eventType match {
      case WatcherAdapter.EventTypeCreate => FileEventKind.Added
      case WatcherAdapter.EventTypeModify => FileEventKind.Modified
      case WatcherAdapter.EventTypeDelete => FileEventKind.Removed
    }
}
