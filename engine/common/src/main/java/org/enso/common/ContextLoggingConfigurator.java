package org.enso.common;

import java.util.ServiceLoader;
import java.util.logging.Handler;
import java.util.logging.Level;
import org.graalvm.polyglot.Context;

/**
 * An SPI interface for a module that can configure loggers and setup {@link Context} to do logging
 * the right way.
 */
public abstract class ContextLoggingConfigurator {
  static final ContextLoggingConfigurator DEFAULT =
      ServiceLoader.load(ContextLoggingConfigurator.class).findFirst().get();

  /**
   * Configures a builder for proper logging.
   *
   * @param builder the builder to configure - using {@link Context.Builder#option(String, String)}
   *     & co.
   * @param logLevel the log level to use for logging
   * @param handlerOrNull explicitly specified handler to use or {@code null} to use default one
   */
  protected abstract void prepareBuilderForLogging(
      Context.Builder builder, Level logLevel, Handler handlerOrNull);
}
