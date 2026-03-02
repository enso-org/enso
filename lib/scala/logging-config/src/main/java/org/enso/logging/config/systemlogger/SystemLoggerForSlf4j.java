package org.enso.logging.config.systemlogger;

import org.slf4j.ILoggerFactory;
import org.slf4j.IMarkerFactory;
import org.slf4j.Logger;
import org.slf4j.Marker;
import org.slf4j.event.Level;
import org.slf4j.spi.MDCAdapter;
import org.slf4j.spi.SLF4JServiceProvider;

/**
 * Bridges slf4j logger to {@link System.Logger}. Needs to be enabled by setting JVM properties.
 * Those are set when booting <em>Other JVM</em> in the <em>dual JVM</em> mode.
 */
public final class SystemLoggerForSlf4j implements SLF4JServiceProvider, ILoggerFactory {

  @Override
  public ILoggerFactory getLoggerFactory() {
    return this;
  }

  @Override
  public IMarkerFactory getMarkerFactory() {
    return null;
  }

  @Override
  public MDCAdapter getMDCAdapter() {
    return null;
  }

  @Override
  public String getRequestedApiVersion() {
    return "2.0.16";
  }

  @Override
  public void initialize() {}

  private System.Logger.Level at(org.slf4j.event.Level l) {
    return switch (l) {
      case org.slf4j.event.Level.ERROR -> System.Logger.Level.ERROR;
      case org.slf4j.event.Level.WARN -> System.Logger.Level.WARNING;
      case org.slf4j.event.Level.INFO -> System.Logger.Level.INFO;
      case org.slf4j.event.Level.DEBUG -> System.Logger.Level.DEBUG;
      case org.slf4j.event.Level.TRACE -> System.Logger.Level.TRACE;
      case null, default -> null;
    };
  }

  @Override
  public Logger getLogger(String name) {
    var delegate = System.getLogger(name);
    return new org.slf4j.helpers.AbstractLogger() {
      @Override
      protected String getFullyQualifiedCallerName() {
        return null;
      }

      @Override
      protected void handleNormalizedLoggingCall(
          Level level,
          Marker marker,
          String messagePattern,
          Object[] arguments,
          Throwable throwable) {
        var at = at(level);
        if (throwable != null) {
          delegate.log(at, messagePattern, throwable);
        } else {
          delegate.log(at, messagePattern);
        }
      }

      @Override
      public boolean isTraceEnabled() {
        return delegate.isLoggable(System.Logger.Level.TRACE);
      }

      @Override
      public boolean isTraceEnabled(Marker marker) {
        return delegate.isLoggable(System.Logger.Level.TRACE);
      }

      @Override
      public boolean isDebugEnabled() {
        return delegate.isLoggable(System.Logger.Level.DEBUG);
      }

      @Override
      public boolean isDebugEnabled(Marker marker) {
        return delegate.isLoggable(System.Logger.Level.DEBUG);
      }

      @Override
      public boolean isInfoEnabled() {
        return delegate.isLoggable(System.Logger.Level.INFO);
      }

      @Override
      public boolean isInfoEnabled(Marker marker) {
        return delegate.isLoggable(System.Logger.Level.INFO);
      }

      @Override
      public boolean isWarnEnabled() {
        return delegate.isLoggable(System.Logger.Level.WARNING);
      }

      @Override
      public boolean isWarnEnabled(Marker marker) {
        return delegate.isLoggable(System.Logger.Level.WARNING);
      }

      @Override
      public boolean isErrorEnabled() {
        return delegate.isLoggable(System.Logger.Level.ERROR);
      }

      @Override
      public boolean isErrorEnabled(Marker marker) {
        return delegate.isLoggable(System.Logger.Level.ERROR);
      }
    };
  }
}
