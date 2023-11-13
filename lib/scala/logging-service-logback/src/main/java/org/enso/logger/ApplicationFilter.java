package org.enso.logger;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.spi.FilterReply;
import org.enso.logger.config.LoggersLevels;

/**
 * An implementation of ch.qos.logback.core.filter.Filter that is created from configuration's and
 * user's custom logger levels.
 *
 * <p>ApplicationFilter combines custom log levels filter with threshold filter.
 */
public class ApplicationFilter extends Filter<ILoggingEvent> {
  private final LoggersLevels loggers;
  private final Level level;
  private final String prefix;

  private final int prefixLength;

  private ApplicationFilter(LoggersLevels loggers, Level level, String prefix) {
    this.loggers = loggers;
    this.level = level;
    this.prefix = prefix;
    this.prefixLength = prefix != null ? prefix.length() + 1 : 0; // inlude `.` in `enso.`
  }

  @Override
  public FilterReply decide(ILoggingEvent event) {
    for (var entry : loggers.entrySet()) {
      if (loggerNameMatches(entry.getKey(), event.getLoggerName())) {
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

  private boolean loggerNameMatches(String validLoggerName, String eventLoggerName) {
    if (prefix != null && eventLoggerName.startsWith(prefix)) {
      return eventLoggerName.substring(prefixLength).startsWith(validLoggerName);
    } else {
      return eventLoggerName.startsWith(validLoggerName);
    }
  }

  public static Filter<ILoggingEvent> fromLoggers(
      LoggersLevels loggers, org.slf4j.event.Level level, String prefix) {
    return new ApplicationFilter(loggers, Level.convertAnSLF4JLevel(level), prefix);
  }
}
