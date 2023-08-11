package org.enso.filewatcher

import java.nio.file.Path

trait WatcherFactory {

  /** Create a instance of [[Watcher]].
    *
    * @param root the directory to watch
    * @param eventCallback the callback handling the file watcher events
    * @param exceptionCallback the callback handling the file watcher errors
    * @return a new instance of [[Watcher]]
    */
  def build(
    root: Path,
    eventCallback: Watcher.WatcherEvent => Unit,
    exceptionCallback: Watcher.WatcherError => Unit
  ): Watcher
}
