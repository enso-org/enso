package org.enso.logger;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.spi.FilterReply;
import org.enso.logger.config.LoggersLevels;

/**
 * An implementation of ch.qos.logback.core.filter.Filter that is created from configuration's and
 * user's custom logger levels.
 */
public class ApplicationFilter extends Filter<ILoggingEvent> {
  private final LoggersLevels loggers;
  private final Level level;

  private ApplicationFilter(LoggersLevels loggers, Level level) {
    this.loggers = loggers;
    this.level = level;
  }

  @Override
  public FilterReply decide(ILoggingEvent event) {
    for (var entry : loggers.entrySet()) {
      if (event.getLoggerName().startsWith(entry.getKey())) {
        Level loggerLevel = Level.convertAnSLF4JLevel(entry.getValue());
        if (event.getLevel().isGreaterOrEqual(loggerLevel)) {

          return FilterReply.NEUTRAL;
        } else {
          return FilterReply.DENY;
        }
      }
    }

    if (event.getLevel().isGreaterOrEqual(level)) {
      return FilterReply.NEUTRAL;
    } else {
      return FilterReply.DENY;
    }
  }

  public static Filter<ILoggingEvent> fromLoggers(
      LoggersLevels loggers, org.slf4j.event.Level level) {
    return new ApplicationFilter(loggers, Level.convertAnSLF4JLevel(level));
  }
}
