package org.enso.logger;

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
    this.forwardLogs = true;
  }

  protected void append(ILoggingEvent e) {
    if (forwardLogs) {
      underlying.doAppend(e);
    } else {
      events.add(e);
    }
  }

  public void reset() {
    this.forwardLogs = true;
    this.underlying.start();
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

  @Override
  public String getName() {
    return NAME;
  }

  public static final String NAME = "memory";
}
