package org.enso.logging;

import java.nio.file.Path;
import org.enso.logger.config.BaseConfig;
import org.slf4j.event.Level;

/**
 * Base class for any logging service that accepts logs from other Enso's services
 *
 * @param <T> the type of the object describing on how to communicate with the service
 */
public abstract class LoggingService<T> {

  /**
   * Starts the service. The `appender` configuration specifies what to do with the received log
   * events.
   *
   * @param level the maximal log level handled by this service
   * @param logRoot the root directory where logs are located
   * @param logPrefix the prefix used in the name of the log file
   * @param config config for the server log target
   * @return
   */
  public abstract T start(Level level, Path logRoot, String logPrefix, BaseConfig config);

  /** Shuts down the service. */
  public abstract void teardown();
}
