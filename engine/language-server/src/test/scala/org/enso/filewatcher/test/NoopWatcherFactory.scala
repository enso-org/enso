package org.enso.filewatcher.test

import org.enso.filewatcher.{Watcher, WatcherFactory}

import java.nio.file.Path
import java.util.function.Consumer

class NoopWatcherFactory extends WatcherFactory {

  override def build(
    root: Path,
    eventCallback: Consumer[Watcher.WatcherEvent],
    exceptionCallback: Consumer[Watcher.WatcherError]
  ): Watcher =
    new NoopWatcher
}
