package org.enso.filewatcher;

import java.nio.file.Path;
import java.util.concurrent.Executor;

/** Interface for directory watcher. */
public interface Watcher extends AutoCloseable {
  void start(Executor executor);

  enum EventType {
    CREATE,
    MODIFY,
    DELETE
  }

  record WatcherEvent(Path path, EventType eventType) {}

  record WatcherError(Throwable throwable) {}
}
