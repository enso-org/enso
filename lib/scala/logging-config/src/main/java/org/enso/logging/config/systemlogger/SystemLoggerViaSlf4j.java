package org.enso.logging.config.systemlogger;

import java.text.MessageFormat;
import java.util.ResourceBundle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Bridges logs sent to {@link System.Logger} to slf4j logger. */
public final class SystemLoggerViaSlf4j extends System.LoggerFinder {
  public SystemLoggerViaSlf4j() {}

  @Override
  public System.Logger getLogger(String name, Module module) {
    var logger = LoggerFactory.getLogger(name);
    return new Bridge(logger);
  }

  private static final class Bridge implements System.Logger {
    private final Logger delegate;

    Bridge(Logger delegate) {
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
        var m = readMsg(bundle, msg);
        delegate.atLevel(at(level)).setCause(thrown).setMessage(m).log();
      }
    }

    @Override
    public void log(Level level, ResourceBundle bundle, String formatOrMessage, Object... params) {
      if (isLoggable(level)) {
        var msg = readMsg(bundle, formatOrMessage);
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
}
