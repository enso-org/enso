package org.enso.logger.config;

import com.typesafe.config.Config;
import java.net.URI;
import java.nio.file.Path;
import org.slf4j.event.Level;

/** Base class for all appenders supported in Enso's config file. */
public abstract class Appender {

  public abstract String getName();

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

  public Boolean setup(Level logLevel, LoggerSetup appenderSetup) {
    return false;
  }

  public Boolean setupForPath(
      Level logLevel, Path logRoot, String logPrefix, LoggerSetup appenderSetup) {
    return setup(logLevel, appenderSetup);
  }

  public Boolean setupForURI(Level logLevel, String hostname, int port, LoggerSetup appenderSetup) {
    return setup(logLevel, appenderSetup);
  }

  public boolean isSameTargetAs(URI uri) {
    return false;
  }

  public static final String defaultPattern =
      "[%level] [%d{yyyy-MM-dd'T'HH:mm:ssXXX}] [%logger] %msg%n";
  protected static final String patternKey = "pattern";
  private static final String nameKey = "name";
}
