package org.enso.logging;

import java.net.URI;
import java.nio.file.Path;
import org.enso.logger.config.Appender;
import org.enso.logger.config.LoggingServer;
import org.slf4j.event.Level;
import scala.concurrent.ExecutionContext;
import scala.concurrent.Future;

public class LoggingServiceManager {
  private static LoggingService<?> loggingService = null;
  private static Level currentLevel = Level.TRACE;

  public static Level currentLogLevelForThisApplication() {
    return currentLevel;
  }

  public static Future<URI> setupServer(
      Level logLevel,
      int port,
      Path logPath,
      String logFileSuffix,
      LoggingServer config,
      ExecutionContext ec) {
    if (loggingService != null) {
      throw new LoggingServiceAlreadySetup();
    } else {
      if (config.appenders().containsKey(config.appender())) {
        currentLevel = logLevel;
        return Future.apply(
            () -> {
              var server = LoggingServiceFactory.get().localServerFor(port);
              loggingService = server;
              Appender appender = config.appenders().get(config.appender());
              return server.start(logLevel, logPath, logFileSuffix, appender);
            },
            ec);
      } else {
        throw new LoggerInitializationFailed();
      }
    }
  }

  public static void teardown() {
    if (loggingService != null) {
      loggingService.teardown();
    }
  }
}
