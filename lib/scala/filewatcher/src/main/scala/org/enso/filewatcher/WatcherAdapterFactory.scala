package org.enso.filewatcher
import java.nio.file.Path

class WatcherAdapterFactory extends WatcherFactory {

  /** @inheritdoc */
  override def build(
    root: Path,
    eventCallback: Watcher.WatcherEvent => Unit,
    exceptionCallback: Watcher.WatcherError => Unit
  ): Watcher =
    new WatcherAdapter(root, eventCallback, exceptionCallback)
}
