package org.enso.logger.config;

import com.typesafe.config.Config;
import java.nio.file.Path;
import org.enso.logger.LoggerSetup;
import org.slf4j.event.Level;

/**
 * Base class for all appenders supported by Enso's logging configuration. Appenders determine what
 * to do with the recorded log events
 */
public abstract sealed class Appender
    permits FileAppender, SocketAppender, SentryAppender, ConsoleAppender {

  /**
   * Returns the name of the appender
   *
   * @return
   */
  public abstract String getName();

  /**
   * Parses config section and returns an appender's configuration.
   *
   * @param config section of the config to parse
   * @return parsed and verified appender configuration
   * @throws MissingConfigurationField if the config file was mis-configured
   */
  public static Appender parse(Config config) throws MissingConfigurationField {
    if (config != null) {
      switch (config.getString(nameKey)) {
        case FileAppender.appenderName:
          return FileAppender.parse(config);
        case SocketAppender.appenderName:
          return SocketAppender.parse(config);
        case SentryAppender.appenderName:
          return SentryAppender.parse(config);
        case ConsoleAppender.appenderName:
          return ConsoleAppender.parse(config);
        default:
          return null;
      }
    }
    return null;
  }

  /**
   * Uses this appender's configuration to setup the logger.
   *
   * @param logLevel maximal level of logs that will be handled by logger
   * @param loggerSetup logger's setup to be used to be invoked with this appender
   * @return true if logger has been setup correctly using this configuration, false otherwise
   */
  public boolean setup(Level logLevel, LoggerSetup loggerSetup) {
    return false;
  }

  /**
   * Uses this appender's configuration to setup the logger.
   *
   * @param logLevel maximal level of logs that will be handled by logger
   * @param loggerSetup logger's setup to be used to be invoked with this appender
   * @return true if logger has been setup correctly using this configuration, false otherwise
   */
  public boolean setupForPath(
      Level logLevel, Path logRoot, String logPrefix, LoggerSetup loggerSetup) {
    return setup(logLevel, loggerSetup);
  }

  public static final String defaultPattern =
      "[%level] [%d{yyyy-MM-dd'T'HH:mm:ssXXX}] [%logger] %msg%n";
  protected static final String patternKey = "pattern";
  private static final String nameKey = "name";
}
