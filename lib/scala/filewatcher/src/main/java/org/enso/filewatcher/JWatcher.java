package org.enso.filewatcher;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.concurrent.Executor;
import scala.Function1;
import scala.runtime.BoxedUnit;

public final class JWatcher implements Watcher {
  private final Path root;
  private final Function1<WatcherEvent, BoxedUnit> eventCallback;
  private final Function1<WatcherError, BoxedUnit> exceptionCallback;
  private WatchKey watchKey;
  private WatchService watchService;
  private boolean stopped = false;

  public JWatcher(
      Path root,
      Function1<WatcherEvent, BoxedUnit> eventCallback,
      Function1<WatcherError, BoxedUnit> exceptionCallback) {
    this.root = root;
    this.eventCallback = eventCallback;
    this.exceptionCallback = exceptionCallback;
  }

  @Override
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

  @Override
  public void stop() {
    stopped = true;
    if (watchService != null) {
      try {
        watchService.close();
      } catch (IOException e) {
        exceptionCallback.apply(new WatcherError(e));
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
            var convertedEvent = new WatcherEvent(absolutePath, eventType);
            eventCallback.apply(convertedEvent);
          }
        }

        var valid = watchKey.reset();
        if (!valid) {
          // object no longer registered
          // TODO: Report exception?
        }
      }
    } catch (Throwable e) {
      var err = new WatcherError(e);
      exceptionCallback.apply(err);
    } finally {
      watchKey.cancel();
    }
  }

  private EventType deduceType(WatchEvent<?> event) {
    return switch (event.kind().name()) {
      case "ENTRY_CREATE" -> EventTypeCreate$.MODULE$;
      case "ENTRY_MODIFY" -> EventTypeModify$.MODULE$;
      case "ENTRY_DELETE" -> EventTypeDelete$.MODULE$;
      default -> {
        var err = new IllegalArgumentException("Unknown event type: " + event.kind());
        var watcherErr = new WatcherError(err);
        exceptionCallback.apply(watcherErr);
        yield null;
      }
    };
  }
}
