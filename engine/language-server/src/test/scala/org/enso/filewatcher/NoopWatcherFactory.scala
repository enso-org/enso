package org.enso.filewatcher
import java.nio.file.Path

class NoopWatcherFactory extends WatcherFactory {

  override def build(
    root: Path,
    eventCallback: Watcher.WatcherEvent => Unit,
    exceptionCallback: Watcher.WatcherError => Unit
  ): Watcher =
    new NoopWatcher
}
