package org.enso.languageserver.filemanager

import java.io.File
import enumeratum._
import org.enso.filewatcher.Watcher

/** A representation of filesystem event.
  *
  * @param path path to the file system object
  * @param kind type of file system event
  * @param attributes the file attributes
  */
case class FileEvent(
  path: Path,
  kind: FileEventKind,
  attributes: Either[FileSystemFailure, FileAttributes]
)

object FileEvent {

  /** Conversion from file system event.
    *
    * @param root a project root
    * @param base a watched path
    * @param event a file system event
    * @param attributes a file attributes
    * @return file event
    */
  def fromWatcherEvent(
    root: File,
    base: Path,
    event: Watcher.WatcherEvent,
    attributes: Either[FileSystemFailure, FileSystemApi.Attributes]
  ): FileEvent = {
    val eventPath = Path.getRelativePath(root, base, event.path)
    FileEvent(
      eventPath,
      FileEventKind(event.eventType),
      attributes.map(
        FileAttributes.fromFileSystemAttributes(root, eventPath, _)
      )
    )
  }
}

/** Type of a file event. */
sealed trait FileEventKind extends EnumEntry

object FileEventKind extends Enum[FileEventKind] with CirceEnum[FileEventKind] {

  /** Event type indicating file creation.
    */
  case object Added extends FileEventKind

  /** Event type indicating file deletion.
    */
  case object Removed extends FileEventKind

  /** Event type indicating file modification.
    */
  case object Modified extends FileEventKind

  override val values = findValues

  /** Create [[FileEventKind]] from [[Watcher.EventType]].
    *
    * @param eventType file system event type
    * @return file event kind
    */
  def apply(eventType: Watcher.EventType): FileEventKind =
    eventType match {
      case Watcher.EventTypeCreate => FileEventKind.Added
      case Watcher.EventTypeModify => FileEventKind.Modified
      case Watcher.EventTypeDelete => FileEventKind.Removed
    }
}
