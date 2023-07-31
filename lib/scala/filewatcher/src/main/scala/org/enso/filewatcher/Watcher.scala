package org.enso.filewatcher

import java.nio.file.Path

trait Watcher {

  /** Start the file watcher. */
  def start(): Unit

  /** Stop the file watcher. */
  def stop(): Unit
}

object Watcher {

  /** Type of a file event. */
  sealed trait EventType

  /** Event type indicating file creation. */
  case object EventTypeCreate extends EventType

  /** Event type indicating file modification. */
  case object EventTypeModify extends EventType

  /** Event type indicating file deletion. */
  case object EventTypeDelete extends EventType

  /** Object representing file system event.
    *
    * @param path path to the file system object
    * @param eventType event type
    */
  case class WatcherEvent(path: Path, eventType: EventType)

  object WatcherEvent {}

  /** Object representing en error.
    *
    * @param exception an error
    */
  case class WatcherError(exception: Exception)
}
