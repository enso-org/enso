package org.enso.logging;

import java.net.URI;
import java.nio.file.Path;
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
        if (config.logToFile().enabled()) {
          String envSetLogLevel = System.getenv("ENSO_LOG_TO_FILE_LOG_LEVEL");
          if (envSetLogLevel != null) {
            currentLevel = config.logToFile().logLevel();
          } else {
            int min = Math.min(config.logToFile().logLevel().toInt(), logLevel.toInt());
            currentLevel = Level.intToLevel(min);
          }
        } else {
          currentLevel = logLevel;
        }
        return Future.apply(
            () -> {
              var server = LoggingServiceFactory.get().localServerFor(port);
              loggingService = server;
              return server.start(logLevel, logPath, logFileSuffix, config);
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
