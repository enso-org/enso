package org.enso.logging;

import java.net.URI;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.enso.logger.config.LoggerSetup;
import org.enso.logger.config.LoggerSetupFactory;
import org.enso.logger.config.MissingConfigurationField;
import org.enso.logger.masking.Masking;
import org.slf4j.event.Level;
import scala.Option;
import scala.Unit$;
import scala.concurrent.Await;
import scala.concurrent.ExecutionContext;
import scala.concurrent.Future;
import scala.concurrent.Promise;
import scala.concurrent.Promise$;
import scala.concurrent.duration.Duration$;

/**
 * Base class for any Enso service that needs to setup its logging.
 *
 * <p>Note: if this looks ugly and not very Java-friendly, it's because it is. It's a 1:1
 * translation from Scala.
 */
public abstract class LoggingSetupHelper {

  public LoggingSetupHelper(ExecutionContext ec) {
    this.ec = ec;
  }

  private ExecutionContext ec;

  protected abstract Level defaultLogLevel();

  protected abstract String logFileSuffix();

  protected abstract Path logPath();

  public Future<Option<URI>> loggingServiceEndpoint() {
    return loggingServiceEndpointPromise.future();
  }

  private Promise<Option<URI>> loggingServiceEndpointPromise = Promise$.MODULE$.apply();

  /**
   * Initialize logging to console prior to establishing logging. Some logs may be added while
   * inferring the parameters of logging infrastructure, leading to catch-22 situations.
   */
  public void initLogger() {
    LoggerSetupFactory.get().setupNoOpAppender();
  }

  public void setupFallback() {
    LoggerSetupFactory.get().setupConsoleAppender(defaultLogLevel());
  }

  /**
   * Starts a logging server that collects logs from different components. Once started, all logs
   * are being forwarded to the server.
   *
   * @param logLevel
   * @param logMasking
   */
  public void setupServerAndForwardLogs(Option<Level> logLevel, boolean logMasking)
      throws MissingConfigurationField {
    var loggerSetup = LoggerSetupFactory.get();
    var config = loggerSetup.getConfig();
    if (config.loggingServerNeedsBoot()) {
      int actualPort = config.getServer().port();
      Level actualLogLevel = logLevel.getOrElse(() -> defaultLogLevel());
      LoggingServiceManager.setupServer(
              actualLogLevel, actualPort, logPath(), logFileSuffix(), config.getServer(), ec)
          .onComplete(
              (result) -> {
                try {
                  if (result.isFailure()) {
                    setup(logLevel, Option.empty(), logMasking, loggerSetup);
                  } else {
                    URI uri = result.get();
                    Masking.setup(logMasking);
                    if (!loggerSetup.setup(actualLogLevel)) {
                      LoggingServiceManager.teardown();
                      loggingServiceEndpointPromise.failure(new LoggerInitializationFailed());
                    } else {
                      loggingServiceEndpointPromise.success(Option.apply(uri));
                    }
                  }
                  return Unit$.MODULE$;
                } catch (MissingConfigurationField e) {
                  throw new RuntimeException(e);
                }
              },
              ec);
    } else {
      // Setup logger according to config
      var actualLogLevel = logLevel.getOrElse(() -> defaultLogLevel());
      if (loggerSetup.setup(actualLogLevel)) {
        loggingServiceEndpointPromise.success(Option.empty());
      }
    }
  }

  public void setup(Option<Level> logLevel, Option<URI> connectToExternalLogger, boolean logMasking)
      throws MissingConfigurationField {
    setup(logLevel, connectToExternalLogger, logMasking, LoggerSetupFactory.get());
  }

  public void setup(
      Option<Level> logLevel,
      Option<URI> connectToExternalLogger,
      boolean logMasking,
      LoggerSetup loggerSetup)
      throws MissingConfigurationField {
    var actualLogLevel = logLevel.getOrElse(() -> defaultLogLevel());
    if (connectToExternalLogger.isDefined()) {
      var uri = connectToExternalLogger.get();
      var initialized =
          loggerSetup.setupSocketAppender(actualLogLevel, uri.getHost(), uri.getPort());
      if (!initialized) {
        // Fallback
        initialized = loggerSetup.setup(actualLogLevel);
        if (!initialized) {
          // Fallback to console
          initialized = loggerSetup.setupConsoleAppender(actualLogLevel);
        }
      }
      if (initialized) {
        Masking.setup(logMasking);
        loggingServiceEndpointPromise.success(Option.empty());
      } else {
        loggingServiceEndpointPromise.failure(new LoggerInitializationFailed());
      }
    } else {
      if (loggerSetup.setup(actualLogLevel)) {
        Masking.setup(logMasking);
        loggingServiceEndpointPromise.success(Option.empty());
      } else {
        loggingServiceEndpointPromise.failure(new LoggerInitializationFailed());
      }
    }
  }

  public void waitForSetup() throws InterruptedException, TimeoutException {
    Await.ready(
        loggingServiceEndpointPromise.future(), Duration$.MODULE$.apply(5, TimeUnit.SECONDS));
  }

  public void tearDown() {
    LoggingServiceManager.teardown();
  }
}
