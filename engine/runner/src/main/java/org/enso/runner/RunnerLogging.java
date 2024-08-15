package org.enso.runner;

import java.net.URI;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.enso.logger.LoggerSetup;
import org.enso.logger.masking.Masking;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;

final class RunnerLogging {
  private RunnerLogging() {}

  private static final Logger logger = LoggerFactory.getLogger(RunnerLogging.class);

  /**
   * Sets up the runner's logging service.
   *
   * <p>If `connectionUri` is provided it tries to connect to a logging service server and pass logs
   * to it. If it is not provided, or the connection could not be established, falls back to logging
   * to standard error output.
   *
   * @param connectionUri optional uri of logging service server to connect to
   * @param logLevel log level to use for the runner and runtime
   * @param logMasking switches log masking on and off
   */
  static void setup(URI connectionUri, Level logLevel, boolean logMasking) {
    Masking.setup(logMasking);
    var loggerSetup = LoggerSetup.get();
    var executorService = Executors.newSingleThreadExecutor();
    try {
      setupImpl(connectionUri, logLevel, executorService, loggerSetup);
    } finally {
      executorService.shutdown();
    }
  }

  private static void setupImpl(
      URI connectionUri, Level logLevel, ExecutorService executorService, LoggerSetup loggerSetup) {
    if (connectionUri != null) {
      var future =
          executorService.submit(
              () ->
                  loggerSetup.setupSocketAppender(
                      logLevel, connectionUri.getHost(), connectionUri.getPort()));
      try {
        var success = future.get();
        if (success) {
          logger.trace("Connected to logging service at [{}]", connectionUri);
          return;
        } else {
          throw new RuntimeException("Failed to connect to logging service");
        }
      } catch (InterruptedException | ExecutionException e) {
        System.err.println(
            "Failed to connect to the logging service server, " + "falling back to local logging.");
      }
    }
    if (!setupConsoleAppender(logLevel, executorService, loggerSetup)) {
      System.err.println("Failed to initialize logging infrastructure");
    }
  }

  static void tearDown() {
    LoggerSetup.get().teardown();
  }

  private static boolean setupConsoleAppender(
      Level logLevel, ExecutorService executorService, LoggerSetup loggerSetup) {
    var future = executorService.submit(() -> loggerSetup.setupConsoleAppender(logLevel));
    try {
      return future.get();
    } catch (ExecutionException | InterruptedException e) {
      System.err.println("Failed to initialize console appender: " + e.getMessage());
      return false;
    }
  }
}
