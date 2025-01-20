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
  private volatile boolean forwardLogs;

  public MemoryAppender(Appender<ILoggingEvent> underlying) {
    this.underlying = underlying;
    this.events = new ArrayList<>();
    this.forwardLogs = underlying != null;
  }

  protected void append(ILoggingEvent e) {
    if (forwardLogs) {
      underlying.doAppend(e);
    } else {
      events.add(e);
    }
  }

  public void reset() {
    this.forwardLogs = underlying != null;
    if (forwardLogs) {
      this.underlying.start();
    }
    events.clear();
  }

  public void flush() {
    // Ensure context set
    underlying.start();
    for (var element : events) {
      underlying.doAppend(element);
    }
    underlying.stop();
  }

  public void stopForwarding() {
    this.forwardLogs = false;
    this.underlying.stop();
  }

  public List<ILoggingEvent> getEvents() {
    return events;
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public String toString() {
    if (this.underlying != null) {
      return "MemoryAppender[forwardTo=" + this.underlying.getName() + "]";
    } else {
      return "MemoryAppender[forwardTo=<disabled>]";
    }
  }

  public static final String NAME = "memory";
}
