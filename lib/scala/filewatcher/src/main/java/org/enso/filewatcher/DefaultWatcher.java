package org.enso.filewatcher;

import java.io.IOException;
import java.nio.file.ClosedWatchServiceException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.function.Consumer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Default directory watcher using JDK's {@link WatchService}. */
final class DefaultWatcher implements Watcher {
  private static final Logger LOGGER = LoggerFactory.getLogger(DefaultWatcher.class);
  private final Path root;
  private final Consumer<Watcher.WatcherEvent> eventCallback;
  private final Consumer<Watcher.WatcherError> exceptionCallback;
  private final Map<Path, WatchKey> watchedDirs = new ConcurrentHashMap<>();
  private final WatchService watchService;
  private boolean closed = false;

  DefaultWatcher(
      Path root,
      Consumer<Watcher.WatcherEvent> eventCallback,
      Consumer<Watcher.WatcherError> exceptionCallback,
      WatchService watchService) {
    this.root = root;
    this.eventCallback = eventCallback;
    this.exceptionCallback = exceptionCallback;
    this.watchService = watchService;
  }

  @Override
  public void start(Executor executor) {
    if (closed) {
      throw new IllegalStateException("Watcher has already been stopped.");
    }
    LOGGER.debug("Starting watcher for root directory {}", root.toAbsolutePath());
    try {
      Files.walkFileTree(
          root,
          new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
              registerWatchService(dir);
              return FileVisitResult.CONTINUE;
            }
          });
    } catch (IOException e) {
      LOGGER.error("Failed to start file watch service in root directory " + root, e);
      exceptionCallback.accept(new Watcher.WatcherError(e));
      return;
    }
    assert watchedDirs.containsKey(root);
    executor.execute(this::eventLoop);
  }

  @Override
  public void close() {
    closed = true;
    try {
      watchService.close();
    } catch (IOException e) {
      exceptionCallback.accept(new Watcher.WatcherError(e));
    }
    for (var watchKey : watchedDirs.values()) {
      watchKey.cancel();
    }
  }

  private void eventLoop() {
    try {
      while (!closed) {
        WatchKey key;
        try {
          // Wait for the next key
          key = watchService.take();
        } catch (InterruptedException e) {
          LOGGER.debug("Watcher service interrupted: {}", e.getMessage());
          var err = new Watcher.WatcherError(e);
          exceptionCallback.accept(err);
          continue;
        } catch (ClosedWatchServiceException e) {
          // ClosedWatchServiceException is a "standard" exception thrown when
          // the watch service is closed. We don't even have to log it.
          return;
        }
        var dir = (Path) key.watchable();
        assert watchedDirs.containsKey(dir)
            : "Directory " + dir + " is not registered in watchedDirs";
        for (var event : key.pollEvents()) {
          dispatchEvent(event, dir);
        }
        var valid = key.reset();
        if (!valid) {
          watchedDirs.remove(dir);
        }
      }
    } catch (Throwable e) {
      var err = new Watcher.WatcherError(e);
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
    LOGGER.trace(
        "Dispatching event: kind={}, path={}, isDir={}, isRepeated={}",
        event.kind(),
        absolutePath,
        isDir,
        isRepeated);
    if (isRepeated) {
      return;
    }
    if (eventType == Watcher.EventType.CREATE && isDir) {
      registerWatchService(absolutePath);
    } else if (eventType == Watcher.EventType.DELETE && isDir) {
      cancelWatch(absolutePath);
    }
    if (eventType != null) {
      var convertedEvent = new Watcher.WatcherEvent(absolutePath, eventType);
      eventCallback.accept(convertedEvent);
    }
  }

  private Watcher.EventType deduceType(WatchEvent<?> event) {
    var kindName = event.kind().name();
    if (kindName.equals(StandardWatchEventKinds.ENTRY_CREATE.name())) {
      return EventType.CREATE;
    } else if (kindName.equals(StandardWatchEventKinds.ENTRY_MODIFY.name())) {
      return EventType.MODIFY;
    } else if (kindName.equals(StandardWatchEventKinds.ENTRY_DELETE.name())) {
      return EventType.DELETE;
    } else if (kindName.equals(StandardWatchEventKinds.OVERFLOW.name())) {
      LOGGER.warn(
          "Received overflow event for path: {}. This may indicate that some events were lost.",
          event.context());
      exceptionCallback(new RuntimeException("Overflow " + event.context()));
      return null;
    } else {
      LOGGER.warn("Received unknown event type: {} for path: {}.", event.kind(), event.context());
      exceptionCallback(new IllegalArgumentException("Unknown event type: " + event.kind()));
      return null;
    }
  }

  private void exceptionCallback(Throwable cause) {
    var watcherError = new Watcher.WatcherError(cause);
    exceptionCallback.accept(watcherError);
  }

  private void registerWatchService(Path dir) {
    LOGGER.debug("Registering watch service for subdir {}", dir);
    try {
      var watchKey =
          dir.register(
              watchService,
              StandardWatchEventKinds.ENTRY_CREATE,
              StandardWatchEventKinds.ENTRY_MODIFY,
              StandardWatchEventKinds.ENTRY_DELETE,
              StandardWatchEventKinds.OVERFLOW);
      watchedDirs.put(dir, watchKey);
    } catch (IOException e) {
      exceptionCallback.accept(new Watcher.WatcherError(e));
    }
  }

  private void cancelWatch(Path dir) {
    LOGGER.debug("Cancelling watch for subdir {}", dir);
    var watchKey = watchedDirs.get(dir);
    assert watchKey != null : "No watch key found for dir: " + dir;
    watchKey.cancel();
    watchedDirs.remove(dir);
  }
}
