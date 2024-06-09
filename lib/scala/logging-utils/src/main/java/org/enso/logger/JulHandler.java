package org.enso.logger;

import static java.util.logging.Level.*;

import java.util.logging.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** java.util.logging.Handler that propagates all events to the equivalent SLF4J implementation. */
public final class JulHandler extends Handler {

  private static final Handler _handler;

  static {
    _handler = new JulHandler();
  }

  private Formatter formattter;

  private JulHandler() {
    this.formattter = new SimpleFormatter();
  }

  public static final Handler get() {
    return _handler;
  }

  @Override
  public void publish(LogRecord record) {
    Logger logger = LoggerFactory.getLogger(record.getLoggerName());
    java.util.logging.Level julLevel = record.getLevel();
    String msg;
    boolean hasThrowable;
    if (record.getThrown() != null) {
      hasThrowable = true;
      msg = formattter.formatMessage(record);
    } else {
      hasThrowable = false;
      msg = record.getMessage().replaceAll("\\{\\d+\\}", "{}");
    }
    if (julLevel.intValue() == SEVERE.intValue()) {
      if (hasThrowable) logger.error(msg, record.getThrown());
      else logger.error(msg, record.getParameters());
    } else if (julLevel.intValue() == INFO.intValue()) {
      if (hasThrowable) logger.info(msg, record.getThrown());
      else logger.info(msg, record.getParameters());
    } else if (julLevel.intValue() == WARNING.intValue()) {
      if (hasThrowable) logger.warn(msg, record.getThrown());
      else logger.warn(msg, record.getParameters());
    } else if (julLevel.intValue() == ALL.intValue()) {
      if (hasThrowable) logger.trace(msg, record.getThrown());
      else logger.trace(msg, record.getParameters());
    } else if (julLevel.intValue() == FINE.intValue()) {
      if (hasThrowable) logger.debug(msg, record.getThrown());
      else logger.debug(msg, record.getParameters());
    } else if (julLevel.intValue() == FINER.intValue()) {
      if (hasThrowable) logger.trace(msg, record.getThrown());
      else logger.trace(msg, record.getParameters());
    } else if (julLevel.intValue() == FINEST.intValue()) {
      if (hasThrowable) logger.trace(msg, record.getThrown());
      else logger.trace(msg, record.getParameters());
    }
  }

  @Override
  public void flush() {}

  @Override
  public void close() throws SecurityException {}
}
