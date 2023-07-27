package org.enso.logger;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.spi.FilterReply;
import org.enso.logger.config.Loggers;

public class ApplicationFilter extends Filter<ILoggingEvent> {
  private final Loggers loggers;

  private ApplicationFilter(Loggers loggers) {
    this.loggers = loggers;
  }

  @Override
  public FilterReply decide(ILoggingEvent event) {
    for (var entry : loggers.entrySet()) {
      if (event.getLoggerName().startsWith(entry.getKey())) {
        if (event.getLevel().isGreaterOrEqual(entry.getValue())) {
          return FilterReply.NEUTRAL;
        } else {
          return FilterReply.DENY;
        }
      }
    }
    return FilterReply.NEUTRAL;
  }

  public static Filter<ILoggingEvent> fromLoggers(Loggers loggers) {
    return new ApplicationFilter(loggers);
  }
}
