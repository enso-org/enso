package org.enso.filewatcher

import java.nio.file.Path
import java.util.concurrent.Executor

trait Watcher {

  /** Start the file watcher.
    *
    * @param executor executor under which this watcher should be run in the background
    */
  def start(executor: Executor): Unit

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
