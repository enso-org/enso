package org.enso.filewatcher;

import java.nio.file.Files;
import java.nio.file.Path;
import org.enso.filewatcher.Watcher.WatcherError;
import org.enso.filewatcher.Watcher.WatcherEvent;
import scala.Function1;
import scala.runtime.BoxedUnit;

public class JDKWatcherFactory implements WatcherFactory {

  @Override
  public Watcher build(
      Path root,
      Function1<WatcherEvent, BoxedUnit> eventCallback,
      Function1<WatcherError, BoxedUnit> exceptionCallback) {
    if (!Files.exists(root) || !Files.isDirectory(root)) {
      throw new IllegalArgumentException(
          "Root path must exist and be a directory: " + root.toAbsolutePath());
    }
    return new JWatcher(root, eventCallback, exceptionCallback);
  }
}
