package org.enso.logger.config;

import java.util.Map;

/** Base config corresponding to the main logger section in the application config. */
public interface BaseConfig {

  /** Returns the default appender. */
  Appender getAppender();

  /** Returns a map of appenders defined in the logger section of the config. */
  Map<String, Appender> getAppenders();

  /** Returns configuration for parallel logging of messages to a log file. */
  LogToFile logToFile();

  /**
   * Returns a list of custom loggers and their levels that need to be taken into account when
   * logging events.
   */
  LoggersLevels getLoggers();
}
