package org.enso.filewatcher;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.WatchService;
import java.util.function.Consumer;
import org.enso.filewatcher.Watcher.WatcherError;
import org.enso.filewatcher.Watcher.WatcherEvent;

final class DefaultWatcherFactory implements WatcherFactory {

  @Override
  public Watcher build(
      Path root, Consumer<WatcherEvent> eventCallback, Consumer<WatcherError> exceptionCallback) {
    if (!Files.exists(root) || !Files.isDirectory(root)) {
      throw new IllegalArgumentException(
          "Root path must exist and be a directory: " + root.toAbsolutePath());
    }
    WatchService watchService;
    try {
      watchService = FileSystems.getDefault().newWatchService();
    } catch (IOException e) {
      throw new IllegalStateException("Failed to create WatchService", e);
    }
    return new DefaultWatcher(root, eventCallback, exceptionCallback, watchService);
  }
}
