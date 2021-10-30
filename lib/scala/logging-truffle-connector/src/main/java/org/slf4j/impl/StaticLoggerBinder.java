package org.slf4j.impl;

import org.enso.truffleloggerwrapper.TruffleLoggerWrapperFactory;
import org.slf4j.ILoggerFactory;

/**
 * Binds the SLF4J logger instance within the runtime to a logger which wraps the TruffleLogger.
 *
 * <p>This way, the standard SLF4J interface that is used in other subprojects, can also be used
 * within the runtime and its log messages are correctly passed to the TruffleLogger. Thus libraries
 * that are used both inside and outside of runtime can keep a simple interface.
 *
 * <p>The public interface of this class must conform to what is expected by an SLF4J backend. See
 * slf4j-simple for reference.
 */
public class StaticLoggerBinder {
  /** Should be in sync with `slf4jVersion` in `build.sbt`. */
  public static String REQUESTED_API_VERSION = "1.7.30";

  private static final StaticLoggerBinder singleton = new StaticLoggerBinder();

  public static StaticLoggerBinder getSingleton() {
    return singleton;
  }

  private final TruffleLoggerWrapperFactory factory = new TruffleLoggerWrapperFactory();
  private final String factoryClassStr = TruffleLoggerWrapperFactory.class.getName();

  public ILoggerFactory getLoggerFactory() {
    return factory;
  }

  public String getLoggerFactoryClassStr() {
    return factoryClassStr;
  }
}
