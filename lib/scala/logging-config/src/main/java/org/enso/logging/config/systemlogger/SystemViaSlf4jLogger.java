package org.enso.logging.config.systemlogger;

import java.text.MessageFormat;
import java.util.ResourceBundle;
import org.slf4j.Logger;

final class SystemViaSlf4jLogger implements System.Logger {
  private final Logger delegate;

  SystemViaSlf4jLogger(Logger delegate) {
    this.delegate = delegate;
  }

  @Override
  public String getName() {
    return delegate.getName();
  }

  @Override
  public boolean isLoggable(Level level) {
    return delegate.isEnabledForLevel(at(level));
  }

  @Override
  public void log(Level level, ResourceBundle bundle, String msg, Throwable thrown) {
    if (isLoggable(level)) {
      java.lang.String m = readMsg(bundle, msg);
      delegate.atLevel(at(level)).setCause(thrown).setMessage(m).log();
    }
  }

  @Override
  public void log(Level level, ResourceBundle bundle, String formatOrMessage, Object... params) {
    if (isLoggable(level)) {
      java.lang.String msg = readMsg(bundle, formatOrMessage);
      if (params != null && params.length > 0) {
        try {
          msg = MessageFormat.format(msg, params);
        } catch (IllegalArgumentException ex) {
          delegate.warn(msg, ex);
        }
      }
      delegate.atLevel(at(level)).log(msg);
    }
  }

  private org.slf4j.event.Level at(Level l) {
    return switch (l) {
      case ERROR -> org.slf4j.event.Level.ERROR;
      case WARNING -> org.slf4j.event.Level.WARN;
      case INFO -> org.slf4j.event.Level.INFO;
      case DEBUG -> org.slf4j.event.Level.DEBUG;
      case TRACE -> org.slf4j.event.Level.TRACE;
      case null -> null;
      default -> org.slf4j.event.Level.intToLevel(l.getSeverity());
    };
  }

  private String readMsg(ResourceBundle bundle, String textOrKey) {
    if (textOrKey == null) {
      return null;
    }
    if (bundle == null) {
      return textOrKey;
    }
    return bundle.getString(textOrKey);
  }
}
