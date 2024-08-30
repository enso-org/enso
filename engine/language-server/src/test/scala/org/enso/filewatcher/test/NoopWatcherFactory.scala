package org.enso.filewatcher.test

import org.enso.filewatcher.{Watcher, WatcherFactory}

import java.nio.file.Path

class NoopWatcherFactory extends WatcherFactory {

  override def build(
    root: Path,
    eventCallback: Watcher.WatcherEvent => Unit,
    exceptionCallback: Watcher.WatcherError => Unit
  ): Watcher =
    new NoopWatcher
}
