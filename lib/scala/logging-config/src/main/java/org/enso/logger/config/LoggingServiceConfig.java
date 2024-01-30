package org.enso.logger.config;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigException;
import com.typesafe.config.ConfigFactory;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
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
    return parseConfig(LoggingServiceConfig.class.getClassLoader());
  }

  /**
   * Parses the logging-service config.
   *
   * @param classLoader A class loader used for loading the configuration resources. If null, the
   *     system class loader will be used
   */
  public static LoggingServiceConfig parseConfig(ClassLoader classLoader)
      throws MissingConfigurationField {
    Objects.requireNonNull(classLoader, "classLoader cannot be null");
    var emptyConf = ConfigFactory.empty().atKey(configurationRoot);
    var defaultRootConf = ConfigFactory.load(classLoader);
    var fallbackRootConf = ConfigFactory.load(LoggingServiceConfig.class.getClassLoader());
    // thread context class loader may be different than class loader for the current class.
    // And the thread context class loader (which is often the same as the system class loader)
    // may not have access to the `application.conf` resource, so we need to load it from the
    // class loader of the current class.
    var root =
        defaultRootConf
            .withFallback(fallbackRootConf)
            .withFallback(emptyConf)
            .getConfig(configurationRoot);
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
        root.hasPath(defaultAppenderKey) ? root.getString(defaultAppenderKey) : "console",
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

  public MemoryAppender getMemoryAppender() {
    return (MemoryAppender) appenders.getOrDefault(MemoryAppender.appenderName, null);
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
