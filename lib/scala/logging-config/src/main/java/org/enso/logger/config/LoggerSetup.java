package org.enso.logger.config;

import java.nio.file.Path;
import org.slf4j.event.Level;

/** Base classes to be implemented by the underlying loggging implementation. */
public abstract class LoggerSetup {

  /** Returns the parsed config used to create this instance * */
  public abstract LoggingServiceConfig getConfig();

  public abstract boolean setupSocketAppender(Level logLevel, String hostname, int port);

  public abstract boolean setupFileAppender(Level logLevel, Path logRoot, String logPrefix);

  public abstract boolean setupConsoleAppender(Level logLevel);

  public abstract boolean setupSentryAppender(Level logLevel);

  public abstract boolean setupNoOpAppender();

  public abstract Boolean setup() throws MissingConfigurationField;

  public abstract Boolean setup(Level logLevel) throws MissingConfigurationField;

  public abstract Boolean setup(
      Level logLevel,
      Path componentLogPath,
      String componentLogPrefix,
      LoggingServiceConfig config);
}
