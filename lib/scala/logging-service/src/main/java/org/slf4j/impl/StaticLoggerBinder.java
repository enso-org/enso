package org.slf4j.impl;

import org.enso.loggingservice.LoggerFactory;
import org.slf4j.ILoggerFactory;

/**
 * Binds the logging service as an SLF4J backend.
 *
 * <p>The public interface of this class must conform to what is expected by an SLF4J backend. See
 * slf4j-simple for reference.
 */
public class StaticLoggerBinder {
  /** Should be in sync with `slf4jVersion` in `build.sbt`. */
  public static String REQUESTED_API_VERSION = "1.7.36";

  private static final StaticLoggerBinder singleton = new StaticLoggerBinder();

  public static StaticLoggerBinder getSingleton() {
    return singleton;
  }

  private final LoggerFactory factory = new LoggerFactory();
  private final String factoryClassStr = LoggerFactory.class.getName();

  public ILoggerFactory getLoggerFactory() {
    return factory;
  }

  public String getLoggerFactoryClassStr() {
    return factoryClassStr;
  }
}
