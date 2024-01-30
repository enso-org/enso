package org.enso.logger;

import java.nio.file.Path;
import java.util.ServiceLoader;
import org.enso.logger.config.LoggingServiceConfig;
import org.enso.logger.config.MissingConfigurationField;
import org.slf4j.event.Level;

/** Base class to be implemented by the underlying logging implementation. */
public abstract class LoggerSetup {
  private static volatile LoggerSetup _instance;
  private static Object _lock = new Object();

  public static LoggerSetup get() {
    LoggerSetup result = _instance;
    if (result == null) {
      synchronized (_lock) {
        result = _instance;
        if (result == null) {
          // Can't initialize in static initializer because Config has to be able to read runtime
          // env vars
          ServiceLoader<LoggerSetup> loader =
              ServiceLoader.load(LoggerSetup.class, LoggerSetup.class.getClassLoader());
          result = loader.findFirst().get();
          _instance = result;
        }
      }
    }
    return result;
  }

  /** Returns parsed application config used to create this instance * */
  public abstract LoggingServiceConfig getConfig();

  /**
   * Setup forwarding of logger's log event to a logging server.
   *
   * @param logLevel the maximal level of logs that will be forwarded
   * @param hostname the name of the host where server is located
   * @param port the port number where server is listening for messages
   * @return true if logger was setup correctly, false otherwise
   */
  public abstract boolean setupSocketAppender(Level logLevel, String hostname, int port);

  /**
   * Setup writing logger's log event to a file.
   *
   * @param logLevel the maximal level of logs that will be written
   * @param logRoot the root directory where logs are located
   * @param logPrefix the prefix used in the name of the log file
   * @return true if logger was setup correctly, false otherwise
   */
  public abstract boolean setupFileAppender(Level logLevel, Path logRoot, String logPrefix);

  /**
   * Setup writing logger's log event to a plain console.
   *
   * @param logLevel the maximal level of logs that will be displayed
   * @return true if logger was setup correctly, false otherwise
   */
  public abstract boolean setupConsoleAppender(Level logLevel);

  /**
   * Setup writing logger's log event to a memory.
   *
   * @param logLevel the maximal level of logs that will be displayed
   * @return true if logger was setup correctly, false otherwise
   */
  public abstract boolean setupMemoryAppender(Level logLevel);

  /**
   * Setup forwarding logger's log event to a sentry,io service. Requires the presence of the
   * sentry's dependency appropriate to the logging implementation.
   *
   * @param logLevel the maximal level of logs that will be displayed
   * @param logRoot the root directory where logs are located
   * @return true if logger was setup correctly, false otherwise
   */
  public abstract boolean setupSentryAppender(Level logLevel, Path logRoot);

  /**
   * Sets up loggers so that all events are being discarded.
   *
   * @return true unconditionally
   */
  public abstract boolean setupNoOpAppender();

  /**
   * Sets up logging according to the application's config file.
   *
   * @return true if logger was setup correctly, false otherwise
   * @throws MissingConfigurationField if application's config has been mis-configured
   */
  public abstract boolean setup() throws MissingConfigurationField;

  /**
   * Sets up logging according to the application's config file while taking into account the
   * provided log level.
   *
   * @param logLevel maximal log level allowed for log events
   * @return true if logger was setup correctly, false otherwise
   * @throws MissingConfigurationField if application's config has been mis-configured
   */
  public abstract boolean setup(Level logLevel) throws MissingConfigurationField;

  /**
   * Sets up logging according to the provided application's config file and log level. If the
   * default logging writes to a file, provided parameters will specify the exact location of the
   * log file. This is more specific than {@link #setup(Level)} method.
   *
   * @param logLevel maximal log level allowed for log events
   * @param logRoot the root directory where logs are located
   * @param logPrefix the prefix used in the name of the log file
   * @oaram config config file to be used to setup loggers (overriding the one returned by {@link
   *     #getConfig()}
   * @return true if logger was setup correctly, false otherwise
   * @throws MissingConfigurationField if application's config has been mis-configured
   */
  public abstract boolean setup(
      Level logLevel, Path logRoot, String logPrefix, LoggingServiceConfig config);

  /** Shuts down all loggers. */
  public abstract void teardown();

  private static final String implClassKey = LoggerSetup.class.getName() + ".impl.class";
}
