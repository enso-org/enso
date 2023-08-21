package org.enso.logger.config;

import com.typesafe.config.Config;
import java.net.URI;
import java.nio.file.Path;
import org.slf4j.event.Level;

public abstract class Appender {

  private final Config raw;

  public Appender(Config raw) {
    this.raw = raw;
  }

  public abstract String getName();

  public static Appender parse(Config config) {
    if (config != null) {
      switch (config.getString("name")) {
        case "file":
          return FileAppender.parse(config);
        case "socket":
          return SocketAppender.parse(config);
        case "sentry":
          return SentryAppender.parse(config);
        default:
          return ConsoleAppender.parse(config);
      }
    }
    return null;
  }

  public Boolean setup(Level logLevel, AppenderSetup appenderSetup) {
    return false;
  }

  public Boolean setupForPath(
      Level logLevel, Path logRoot, String logPrefix, AppenderSetup appenderSetup) {
    return false;
  }

  public Boolean setupForURI(
      Level logLevel, String hostname, int port, AppenderSetup appenderSetup) {
    return false;
  }

  public boolean isSameTargetAs(URI uri) {
    return false;
  }

  public Config getConfig() {
    return this.raw;
  }
}
