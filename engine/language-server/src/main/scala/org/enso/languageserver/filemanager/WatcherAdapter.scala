package org.enso.languageserver.filemanager

import java.nio.file.Path

import io.methvin.watcher._
import zio._

/** Watches the root with subdirectories and executes callback on file event.
  *
  * @param root directory to watch
  * @param eventCallback callback that fires on the file system events
  * @param errorCallback callback that fires on a watcher error
  */
final class WatcherAdapter(
  root: Path,
  eventCallback: WatcherAdapter.WatcherEvent => Unit,
  errorCallback: WatcherAdapter.WatcherError => Unit
) extends DirectoryChangeListener {

  import WatcherAdapter._

  private val watcher: DirectoryWatcher = DirectoryWatcher
    .builder()
    .path(root)
    .listener(this)
    .build()

  /** Start watcher.
    */
  def start(): IO[Throwable, Unit] =
    IO(watcher.watch())

  /** Stop watcher.
    */
  def stop(): IO[Throwable, Unit] =
    IO(watcher.close())

  /** A callback executed by `DirectoryWatcher` on file system event.
    */
  override def onEvent(event: DirectoryChangeEvent): Unit = {
    WatcherEvent
      .from(event)
      .foreach(eventCallback)
  }

  override def onException(e: Exception): Unit = {
    errorCallback(WatcherAdapter.WatcherError(e))
  }
}

object WatcherAdapter {

  /** Type of a file event.
    */
  sealed trait EventType

  private object EventType {

    /** Creates [[EventType]] from file system event type.
      *
      * @param eventType file system event type
      * @return watcher event type
      */
    def from(eventType: DirectoryChangeEvent.EventType): Option[EventType] =
      eventType match {
        case DirectoryChangeEvent.EventType.CREATE   => Some(EventTypeCreate)
        case DirectoryChangeEvent.EventType.MODIFY   => Some(EventTypeModify)
        case DirectoryChangeEvent.EventType.DELETE   => Some(EventTypeDelete)
        case DirectoryChangeEvent.EventType.OVERFLOW => None
      }
  }

  /** Event type indicating file creation.
    */
  case object EventTypeCreate extends EventType

  /** Event type indicating file modification.
    */
  case object EventTypeModify extends EventType

  /** Event type indicating file deletion.
    */
  case object EventTypeDelete extends EventType

  /** Object representing file system event.
    *
    * @param path path to the file system object
    * @param eventType event type
    */
  case class WatcherEvent(path: Path, eventType: EventType)

  object WatcherEvent {

    /** Conversion form file system event to [[WatcherEvent]]
      *
      * @param event file system event
      * @return watcher event
      */
    def from(event: DirectoryChangeEvent): Option[WatcherEvent] =
      EventType
        .from(event.eventType())
        .map(WatcherEvent(event.path(), _))
  }

  /** Object representing en error.
    *
    * @param exception an error
    */
  case class WatcherError(exception: Exception)

  def build(
    root: Path,
    eventCallback: WatcherEvent => Unit,
    exceptionCallback: WatcherError => Unit
  ): WatcherAdapter =
    new WatcherAdapter(root, eventCallback, exceptionCallback)

}
