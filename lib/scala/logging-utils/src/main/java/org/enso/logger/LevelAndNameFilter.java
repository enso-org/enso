package org.enso.logger;

import java.util.logging.Level;

public interface LevelAndNameFilter {
  /**
   * Returns true if log event should be logged, given the current log configuration.
   *
   * @param level log level of the event to be logged
   * @param name name of the logger of the event to be logged
   * @return true if the log event should be logged, false otherwise
   */
  boolean isLoggable(Level level, String name);
}
