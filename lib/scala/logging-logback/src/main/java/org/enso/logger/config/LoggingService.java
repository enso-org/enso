package org.enso.logger.config;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigException;
import com.typesafe.config.ConfigFactory;
import java.util.Optional;

public class LoggingService {
  public static final String configurationRoot = "logging-service";
  public static final String serverKey = "server";
  public static final String loggersKey = "logger";
  public static final String appenderKey = "appender";
  public static final String testLogLevelKey = "test-log-level";

  private Loggers loggers;
  private String appender;
  private Optional<String> testLogLevel;
  private Server server;

  private LoggingService(
      Loggers loggers, Optional<String> testLogLevel, String appender, Server server) {
    this.loggers = loggers;
    this.appender = appender;
    this.testLogLevel = testLogLevel;
    this.server = server;
  }

  public static LoggingService parseConfig() {
    var empty = ConfigFactory.empty().atKey(configurationRoot);
    var root = ConfigFactory.load().withFallback(empty).getConfig(configurationRoot);
    Server server = Server.parse(root.getConfig(serverKey));
    Loggers loggers = Loggers.parse(root.getConfig(loggersKey));
    return new LoggingService(
        loggers, getStringOpt(testLogLevelKey, root), root.getString(appenderKey), server);
  }

  public Loggers getLoggers() {
    return loggers;
  }

  public String getAppender() {
    return appender;
  }

  private static Optional<String> getStringOpt(String key, Config config) {
    try {
      return Optional.ofNullable(config.getString(key));
    } catch (ConfigException.Missing missing) {
      return Optional.empty();
    }
  }

  public Optional<String> getTestLogLevel() {
    return testLogLevel;
  }

  public Server getServer() {
    return server;
  }

  @Override
  public String toString() {
    return "Loggers: "
        + loggers
        + ", appender: "
        + appender
        + ", testLogLevel: "
        + testLogLevel.orElseGet(() -> "unknown")
        + ", server: "
        + server;
  }
}
