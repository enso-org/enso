package org.enso.filewatcher;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.concurrent.Executor;
import java.util.function.Consumer;
import org.enso.filewatcher.JWatcherEvent.EventType;

public final class JWatcher {
  private final Path root;
  private final Consumer<JWatcherEvent> eventCallback;
  private final Consumer<JWatcherError> exceptionCallback;
  private WatchKey watchKey;
  private WatchService watchService;
  private boolean stopped = false;

  public static JWatcher create(
      Path root, Consumer<JWatcherEvent> eventCallback, Consumer<JWatcherError> exceptionCallback) {
    if (!Files.exists(root) || !Files.isDirectory(root)) {
      throw new IllegalArgumentException(
          "Root path must exist and be a directory: " + root.toAbsolutePath());
    }
    return new JWatcher(root, eventCallback, exceptionCallback);
  }

  private JWatcher(
      Path root, Consumer<JWatcherEvent> eventCallback, Consumer<JWatcherError> exceptionCallback) {
    this.root = root;
    this.eventCallback = eventCallback;
    this.exceptionCallback = exceptionCallback;
  }

  public void start(Executor executor) {
    if (stopped) {
      throw new IllegalStateException("Watcher has already been stopped.");
    }
    System.out.println("[JWatcher] Starting watcher for root " + root.toAbsolutePath());
    try {
      watchService = FileSystems.getDefault().newWatchService();
      watchKey =
          root.register(
              watchService,
              StandardWatchEventKinds.ENTRY_MODIFY,
              StandardWatchEventKinds.ENTRY_CREATE,
              StandardWatchEventKinds.ENTRY_DELETE,
              StandardWatchEventKinds.OVERFLOW);
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
    executor.execute(() -> watch(watchKey));
  }

  public void stop() {
    stopped = true;
    if (watchService != null) {
      try {
        watchService.close();
      } catch (IOException e) {
        exceptionCallback.accept(new JWatcherError(e));
      }
    }
  }

  private void watch(WatchKey watchKey) {
    try {
      while (!stopped) {
        for (var event : watchKey.pollEvents()) {
          var eventPath = (Path) event.context();
          System.out.println(
              "[JWatcher] Received event, kind:" + event.kind() + " for path " + eventPath);
          var absolutePath = root.resolve(eventPath);
          var eventType = deduceType(event);
          if (eventType != null) {
            var convertedEvent = new JWatcherEvent(absolutePath, eventType);
            eventCallback.accept(convertedEvent);
          }
        }

        var valid = watchKey.reset();
        if (!valid) {
          // object no longer registered
          // TODO: Report exception?
        }
      }
    } catch (Throwable e) {
      var err = new JWatcherError(e);
      exceptionCallback.accept(err);
    } finally {
      watchKey.cancel();
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

  public record JWatcherError(Throwable throwable) {}
}
