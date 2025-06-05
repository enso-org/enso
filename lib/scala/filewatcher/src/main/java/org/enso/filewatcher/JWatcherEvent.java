package org.enso.filewatcher;

import java.nio.file.Path;

public record JWatcherEvent(Path path, EventType type) {
  public enum EventType {
    CREATE,
    MODIFY,
    DELETE
  }
}
