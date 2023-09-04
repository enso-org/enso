package org.enso.filewatcher

import io.methvin.watcher._

import java.nio.file.Path

/** Watches the root with subdirectories and executes callback on file event.
  *
  * @param root directory to watch
  * @param eventCallback callback that fires on the file system events
  * @param errorCallback callback that fires on a watcher error
  */
final class WatcherAdapter(
  root: Path,
  eventCallback: Watcher.WatcherEvent => Unit,
  errorCallback: Watcher.WatcherError => Unit
) extends Watcher
    with DirectoryChangeListener {

  private val watcher: DirectoryWatcher = DirectoryWatcher
    .builder()
    .path(root)
    .listener(this)
    .build()

  /** @inheritdoc */
  override def start(): Unit = {
    watcher.watch()
  }

  /** @inheritdoc */
  override def stop(): Unit = {
    watcher.close()
  }

  /** A callback executed by `DirectoryWatcher` on file system event. */
  override def onEvent(event: DirectoryChangeEvent): Unit = {
    WatcherAdapter
      .watcherEvent(event)
      .foreach(eventCallback)
  }

  override def onException(e: Exception): Unit = {
    errorCallback(Watcher.WatcherError(e))
  }
}

object WatcherAdapter {

  /** Creates [[Watcher.EventType]] from file system event type.
    *
    * @param eventType file system event type
    * @return watcher event type
    */
  private def eventType(
    eventType: DirectoryChangeEvent.EventType
  ): Option[Watcher.EventType] =
    eventType match {
      case DirectoryChangeEvent.EventType.CREATE =>
        Some(Watcher.EventTypeCreate)
      case DirectoryChangeEvent.EventType.MODIFY =>
        Some(Watcher.EventTypeModify)
      case DirectoryChangeEvent.EventType.DELETE =>
        Some(Watcher.EventTypeDelete)
      case DirectoryChangeEvent.EventType.OVERFLOW => None
    }

  /** Conversion form file system event to [[Watcher.WatcherEvent]]
    *
    * @param event file system event
    * @return watcher event
    */
  private def watcherEvent(
    event: DirectoryChangeEvent
  ): Option[Watcher.WatcherEvent] =
    eventType(event.eventType())
      .map(Watcher.WatcherEvent(event.path(), _))

}
