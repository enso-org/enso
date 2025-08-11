package org.enso.logging.service.logback;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.Appender;
import ch.qos.logback.core.AppenderBase;
import java.util.ArrayList;
import java.util.List;

/**
 * Appender that keeps all log events internally and can either discard them or forward to the
 * underlying appender.
 */
public class MemoryAppender extends AppenderBase<ILoggingEvent> {
  private final Appender<ILoggingEvent> underlying;

  private final List<ILoggingEvent> events;
  private volatile boolean canForwardLogs;

  public MemoryAppender(Appender<ILoggingEvent> underlying) {
    this.underlying = underlying;
    this.events = new ArrayList<>();
    this.canForwardLogs = underlying != null;
  }

  protected void append(ILoggingEvent e) {
    events.add(e);
  }

  public void reset() {
    events.clear();
  }

  public void flush() {
    if (canForwardLogs) {
      for (var element : events) {
        underlying.doAppend(element);
      }
    }
  }

  public List<ILoggingEvent> getEvents() {
    return new ArrayList<>(events);
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public String toString() {
    if (canForwardLogs) {
      return "MemoryAppender[forwardTo=" + this.underlying.getName() + "]";
    } else {
      return "MemoryAppender[forwardTo=<disabled>]";
    }
  }

  public static final String NAME = "memory";
}
