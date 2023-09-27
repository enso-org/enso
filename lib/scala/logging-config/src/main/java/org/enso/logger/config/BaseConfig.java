package org.enso.logger.config;

import java.util.Map;

/** Base config corresponding to the main logger section in the application config. */
public interface BaseConfig {

  /** Returns the default appender. */
  Appender getAppender();

  /** Returns a map of appenders defined in the logger section of the config. */
  Map<String, Appender> getAppenders();

  /**
   * Returns true, if logging infrastructure should always log in verbose mode, irrespective of the
   * log target.
   */
  boolean logToFile();

  /**
   * Returns a list of custom loggers and their levels that need to be taken into account when
   * logging events.
   */
  LoggersLevels getLoggers();
}
