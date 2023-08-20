package org.enso.logger.config;

import com.typesafe.config.Config;

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
        default:
          return ConsoleAppender.parse(config);
      }
    }
    return null;
  }

  public Config getConfig() {
    return this.raw;
  }
}
