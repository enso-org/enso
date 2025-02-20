package org.enso.languageserver.filemanager

import io.circe.{Decoder, Encoder}

import java.io.File
import org.enso.filewatcher.Watcher

/** A representation of filesystem event.
  *
  * @param path path to the file system object
  * @param kind type of file system event
  * @param attributes the file attributes
  */
case class FileEvent(
  path: Path,
  kind: FileEventKinds.FileEventKind,
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
      FileEventKinds(event.eventType),
      attributes.map(
        FileAttributes.fromFileSystemAttributes(root, eventPath, _)
      )
    )
  }
}

/** Type of a file event. */
object FileEventKinds extends Enumeration {

  type FileEventKind = Value

  /** Added - event type indicating file creation.
    * Removed - event type indicating file deletion.
    * Modified - event type indicating file modification.
    */
  val Added, Removed, Modified = Value

  /** Create [[FileEventKind]] from [[Watcher.EventType]].
    *
    * @param eventType file system event type
    * @return file event kind
    */
  def apply(eventType: Watcher.EventType): FileEventKind =
    eventType match {
      case Watcher.EventTypeCreate => Added
      case Watcher.EventTypeModify => Modified
      case Watcher.EventTypeDelete => Removed
    }

  implicit val genderDecoder: Decoder[FileEventKind] =
    Decoder.decodeEnumeration(FileEventKinds)
  implicit val genderEncoder: Encoder[FileEventKind] =
    Encoder.encodeEnumeration(FileEventKinds)
}
