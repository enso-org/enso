package org.enso.jvm.interop.impl;

import java.util.Arrays;
import org.slf4j.ILoggerFactory;
import org.slf4j.IMarkerFactory;
import org.slf4j.Logger;
import org.slf4j.Marker;
import org.slf4j.event.Level;
import org.slf4j.helpers.AbstractLogger;
import org.slf4j.spi.MDCAdapter;

public final class OtherJvmLogger implements org.slf4j.spi.SLF4JServiceProvider {
  private static ILoggerFactory delegate;

  public static void registerLoggerFactory(ILoggerFactory factory) {
    delegate = factory;
  }

  @Override
  public ILoggerFactory getLoggerFactory() {
    return delegate;
  }

  @Override
  public IMarkerFactory getMarkerFactory() {
    throw new UnsupportedOperationException("getMarkerFactory");
  }

  @Override
  public MDCAdapter getMDCAdapter() {
    throw new UnsupportedOperationException("getMDCAdapter");
  }

  @Override
  public String getRequestedApiVersion() {
    return "2.0.1";
  }

  @Override
  public void initialize() {}

  private static class OtherJvmFactory implements ILoggerFactory {
    public OtherJvmFactory() {}

    @Override
    public Logger getLogger(String name) {
      return new LoggerImpl(name);
    }
  }

  private static class LoggerImpl extends AbstractLogger {
    LoggerImpl(String name) {
      this.name = name;
    }

    @Override
    protected String getFullyQualifiedCallerName() {
      return name;
    }

    @Override
    protected void handleNormalizedLoggingCall(
        Level level,
        Marker marker,
        String messagePattern,
        Object[] arguments,
        Throwable throwable) {
      System.err.println(
          "handleNormalizedLoggingCall: "
              + level
              + " messagePattern "
              + messagePattern
              + " args: "
              + Arrays.toString(arguments)
              + " th: "
              + throwable);
    }

    @Override
    public boolean isTraceEnabled() {
      return false;
    }

    @Override
    public boolean isTraceEnabled(Marker marker) {
      return false;
    }

    @Override
    public boolean isDebugEnabled() {
      return false;
    }

    @Override
    public boolean isDebugEnabled(Marker marker) {
      return false;
    }

    @Override
    public boolean isInfoEnabled() {
      return false;
    }

    @Override
    public boolean isInfoEnabled(Marker marker) {
      return false;
    }

    @Override
    public boolean isWarnEnabled() {
      return true;
    }

    @Override
    public boolean isWarnEnabled(Marker marker) {
      return true;
    }

    @Override
    public boolean isErrorEnabled() {
      return true;
    }

    @Override
    public boolean isErrorEnabled(Marker marker) {
      return true;
    }
  }
}
