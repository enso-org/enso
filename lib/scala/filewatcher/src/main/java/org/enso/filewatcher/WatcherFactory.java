package org.enso.filewatcher;

import java.nio.file.Path;
import java.util.function.Consumer;

/** Factory for directory watcher */
public interface WatcherFactory {

  static WatcherFactory createDefault() {
    return new DefaultWatcherFactory();
  }

  /**
   * @param root Must be existing directory
   */
  Watcher build(
      Path root,
      Consumer<Watcher.WatcherEvent> eventCallback,
      Consumer<Watcher.WatcherError> exceptionCallback);
}
