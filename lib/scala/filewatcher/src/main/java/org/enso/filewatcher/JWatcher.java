package org.enso.filewatcher;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.function.Consumer;
import org.enso.filewatcher.JWatcherEvent.EventType;

public final class JWatcher implements AutoCloseable {
  private final Path root;
  private final Consumer<JWatcherEvent> eventCallback;
  private final Consumer<JWatcherError> exceptionCallback;
  private final Map<Path, WatchKey> watchedDirs = new ConcurrentHashMap<>();
  private final WatchService watchService;
  private boolean closed = false;

  public static JWatcher create(
      Path root, Consumer<JWatcherEvent> eventCallback, Consumer<JWatcherError> exceptionCallback) {
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
    return new JWatcher(root, eventCallback, exceptionCallback, watchService);
  }

  private JWatcher(
      Path root,
      Consumer<JWatcherEvent> eventCallback,
      Consumer<JWatcherError> exceptionCallback,
      WatchService watchService) {
    this.root = root;
    this.eventCallback = eventCallback;
    this.exceptionCallback = exceptionCallback;
    this.watchService = watchService;
  }

  public void start(Executor executor) {
    if (closed) {
      throw new IllegalStateException("Watcher has already been stopped.");
    }
    System.out.println("[JWatcher] Starting watcher for root " + root.toAbsolutePath());
    try {
      var watchKey =
          root.register(
              watchService,
              StandardWatchEventKinds.ENTRY_MODIFY,
              StandardWatchEventKinds.ENTRY_CREATE,
              StandardWatchEventKinds.ENTRY_DELETE,
              StandardWatchEventKinds.OVERFLOW);
      watchedDirs.put(root, watchKey);
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
    executor.execute(this::eventLoop);
  }

  @Override
  public void close() {
    closed = true;
    try {
      watchService.close();
    } catch (IOException e) {
      exceptionCallback.accept(new JWatcherError(e));
    }
    for (var watchKey : watchedDirs.values()) {
      watchKey.cancel();
    }
  }

  private void eventLoop() {
    try {
      while (!closed) {
        var iterator = watchedDirs.entrySet().iterator();
        while (iterator.hasNext()) {
          var entry = iterator.next();
          var dir = entry.getKey();
          var watchKey = entry.getValue();
          for (var event : watchKey.pollEvents()) {
            dispatchEvent(event, dir);
          }
          var valid = watchKey.reset();
          if (!valid) {
            iterator.remove();
          }
        }
      }
    } catch (Throwable e) {
      var err = new JWatcherError(e);
      exceptionCallback.accept(err);
    }
  }

  /**
   * @param event
   * @param dir The directory where the event occurred. This directory must have already been
   *     registered by the watchservice.
   */
  private void dispatchEvent(WatchEvent<?> event, Path dir) {
    var eventPath = (Path) event.context();
    var absolutePath = dir.resolve(eventPath);
    var isDir = Files.isDirectory(absolutePath);
    var isRepeated = event.count() > 1;
    var eventType = deduceType(event);
    System.out.println(
        "[JWatcher] Received event, kind:"
            + event.kind()
            + ", path: "
            + absolutePath
            + ", isDir: "
            + isDir
            + ", isRepeated: "
            + isRepeated);
    if (isRepeated) {
      return;
    }
    if (eventType == EventType.CREATE && isDir) {
      registerWatchService(absolutePath);
    } else if (eventType == EventType.DELETE && isDir) {
      cancelWatch(absolutePath);
    }
    if (eventType != null) {
      var convertedEvent = new JWatcherEvent(absolutePath, eventType);
      eventCallback.accept(convertedEvent);
    }
  }

  private EventType deduceType(WatchEvent<?> event) {
    return switch (event.kind().name()) {
      case "ENTRY_CREATE" -> EventType.CREATE;
      case "ENTRY_MODIFY" -> EventType.MODIFY;
      case "ENTRY_DELETE" -> EventType.DELETE;
      default -> {
        var err = new IllegalArgumentException("Unknown event type: " + event.kind());
        var watcherErr = new JWatcherError(err);
        exceptionCallback.accept(watcherErr);
        yield null;
      }
    };
  }

  private void registerWatchService(Path path) {
    System.out.println("[JWatcher] Registering watch service for " + path);
    try {
      var watchKey =
          path.register(
              watchService,
              StandardWatchEventKinds.ENTRY_CREATE,
              StandardWatchEventKinds.ENTRY_MODIFY,
              StandardWatchEventKinds.ENTRY_DELETE,
              StandardWatchEventKinds.OVERFLOW);
      watchedDirs.put(path, watchKey);
    } catch (IOException e) {
      exceptionCallback.accept(new JWatcherError(e));
    }
  }

  private void cancelWatch(Path path) {
    System.out.println("[JWatcher] Cancelling watch for " + path);
    var watchKey = watchedDirs.get(path);
    assert watchKey != null : "No watch key found for path: " + path;
    watchKey.cancel();
    watchedDirs.remove(path);
  }

  public record JWatcherError(Throwable throwable) {}
}
