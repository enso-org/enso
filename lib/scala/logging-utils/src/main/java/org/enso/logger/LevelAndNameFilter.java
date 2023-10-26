package org.enso.logger;

import java.util.logging.Level;

public interface LevelAndNameFilter {
  boolean isLoggable(Level level, String name);
}
