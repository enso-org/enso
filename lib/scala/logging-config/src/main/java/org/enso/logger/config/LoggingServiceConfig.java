package org.enso.logger.config;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigException;
import com.typesafe.config.ConfigFactory;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Parsed and verified representation of `logging-service` section of `application.conf`. Defines
 * custom log levels, logging appenders and, optionally, logging server configuration.
 */
public class LoggingServiceConfig implements BaseConfig {
  public static final String configurationRoot = "logging-service";
  public static final String serverKey = "server";
  public static final String loggersKey = "logger";
  public static final String appendersKey = "appenders";
  public static final String defaultAppenderKey = "default-appender";
  public static final String logLevelKey = "log-level";

  public static final String logToFileKey = "log-to-file";
  private final LoggersLevels loggers;
  private final Map<String, Appender> appenders;

  private final String defaultAppenderName;
  private final LogToFile logToFile;
  private final Optional<String> logLevel;
  private final LoggingServer server;

  private LoggingServiceConfig(
      LoggersLevels loggers,
      Optional<String> logLevel,
      Map<String, Appender> appenders,
      String defaultAppender,
      LogToFile logTo,
      LoggingServer server) {
    this.loggers = loggers;
    this.appenders = appenders;
    this.defaultAppenderName = defaultAppender;
    this.logToFile = logTo;
    this.logLevel = logLevel;
    this.server = server;
  }

  public static LoggingServiceConfig parseConfig() throws MissingConfigurationField {
    var empty = ConfigFactory.empty().atKey(configurationRoot);
    var root = ConfigFactory.load().withFallback(empty).getConfig(configurationRoot);
    LoggingServer server;
    if (root.hasPath(serverKey)) {
      Config serverConfig = root.getConfig(serverKey);
      server = LoggingServer.parse(serverConfig);
    } else {
      server = null;
    }
    Map<String, Appender> appendersMap = new HashMap<>();
    if (root.hasPath(appendersKey)) {
      List<? extends Config> configs = root.getConfigList(appendersKey);
      for (Config c : configs) {
        Appender a = Appender.parse(c);
        appendersMap.put(a.getName(), a);
      }
    }
    LoggersLevels loggers;
    if (root.hasPath(loggersKey)) {
      loggers = LoggersLevels.parse(root.getConfig(loggersKey));
    } else {
      loggers = LoggersLevels.parse();
    }
    LogToFile logToFile =
        root.hasPath(logToFileKey)
            ? LogToFile.fromConfig(root.getConfig(logToFileKey))
            : LogToFile.disabled();
    return new LoggingServiceConfig(
        loggers,
        getStringOpt(logLevelKey, root),
        appendersMap,
        root.getString(defaultAppenderKey),
        logToFile,
        server);
  }

  public static LoggingServiceConfig withSingleAppender(BaseConfig config) {
    Map<String, Appender> map = config.getAppenders();
    return new LoggingServiceConfig(
        LoggersLevels.parse(),
        Optional.empty(),
        map,
        config.getAppender().getName(),
        config.logToFile(),
        null);
  }

  public LoggersLevels getLoggers() {
    return loggers;
  }

  @Override
  public Appender getAppender() {
    return appenders.get(defaultAppenderName);
  }

  @Override
  public Map<String, Appender> getAppenders() {
    return appenders;
  }

  public SocketAppender getSocketAppender() {
    return (SocketAppender) appenders.getOrDefault(SocketAppender.appenderName, null);
  }

  public FileAppender getFileAppender() {
    return (FileAppender) appenders.getOrDefault(FileAppender.appenderName, null);
  }

  public ConsoleAppender getConsoleAppender() {
    return (ConsoleAppender) appenders.getOrDefault(ConsoleAppender.appenderName, null);
  }

  public SentryAppender getSentryAppender() {
    return (SentryAppender) appenders.getOrDefault(SentryAppender.appenderName, null);
  }

  public boolean loggingServerNeedsBoot() {
    return server != null && server.start();
  }

  private static Optional<String> getStringOpt(String key, Config config) {
    try {
      return Optional.ofNullable(config.getString(key));
    } catch (ConfigException.Missing missing) {
      return Optional.empty();
    }
  }

  public Optional<String> getLogLevel() {
    return logLevel;
  }

  public LoggingServer getServer() {
    return server;
  }

  @Override
  public LogToFile logToFile() {
    return logToFile;
  }

  @Override
  public String toString() {
    return "Loggers: "
        + loggers
        + ", appenders: "
        + String.join(",", appenders.keySet())
        + ", default-appender: "
        + (defaultAppenderName == null ? "unknown" : defaultAppenderName)
        + ", logLevel: "
        + logLevel.orElseGet(() -> "default")
        + ", log-to-file: "
        + logToFile()
        + ", server: "
        + server;
  }
}
