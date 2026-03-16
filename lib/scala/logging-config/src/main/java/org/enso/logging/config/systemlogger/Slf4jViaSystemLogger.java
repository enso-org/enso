package org.enso.logging.config.systemlogger;

import org.slf4j.Marker;
import org.slf4j.event.Level;
import org.slf4j.helpers.AbstractLogger;

final class Slf4jViaSystemLogger extends AbstractLogger {
  private final System.Logger delegate;

  Slf4jViaSystemLogger(System.Logger delegate) {
    this.delegate = delegate;
  }

  @Override
  protected String getFullyQualifiedCallerName() {
    return null;
  }

  @Override
  protected void handleNormalizedLoggingCall(
      Level level, Marker marker, String msgOrPattern, Object[] arguments, Throwable throwable) {
    java.lang.System.Logger.Level at = at(level);
    if (throwable != null) {
      if (arguments != null && arguments.length > 0) {
        var jdkMsg =
            org.slf4j.helpers.MessageFormatter.arrayFormat(msgOrPattern, arguments).getMessage();
        delegate.log(at, jdkMsg, throwable);
      } else {
        delegate.log(at, msgOrPattern, throwable);
      }
    } else {
      if (arguments != null && arguments.length > 0) {
        var jdkPattern = toJdkPattern(msgOrPattern);
        delegate.log(at, jdkPattern, arguments);
      } else {
        delegate.log(at, msgOrPattern);
      }
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

  private static String toJdkPattern(String pattern) {
    int from = 0;
    int count = 0;
    for (; ; ) {
      var found = pattern.indexOf("{}", from);
      if (found == -1) {
        break;
      }
      var newPrefix = pattern.substring(0, found) + "{" + count++ + "}";
      from = newPrefix.length();
      pattern = newPrefix + pattern.substring(found + 2);
    }
    return pattern;
  }
}
