package org.enso.logger.config;

import com.typesafe.config.ConfigFactory;

public class LoggingService {
  public static final String configurationRoot = "logging-service";
  public static final String serverKey = "server";
  public static final String loggersKey = "logger";

  public static final String appenderKey = "appender";

  private Loggers loggers;
  private String appender;
  private Server server;

  private LoggingService(Loggers loggers, String appender, Server server) {
    this.loggers = loggers;
    this.appender = appender;
    this.server = server;
  }

  public static LoggingService parseConfig() {
    var empty = ConfigFactory.empty().atKey(configurationRoot);
    var root = ConfigFactory.load().withFallback(empty).getConfig(configurationRoot);
    Server server = Server.parse(root.getConfig(serverKey));
    Loggers loggers = Loggers.parse(root.getConfig(loggersKey));
    return new LoggingService(loggers, root.getString(appenderKey), server);
  }

  public Loggers getLoggers() {
    return loggers;
  }

  public String getAppender() {
    return appender;
  }

  public Server getServer() {
    return server;
  }
}
