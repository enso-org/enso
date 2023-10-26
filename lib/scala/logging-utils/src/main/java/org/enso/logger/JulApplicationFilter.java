package org.enso.logger;

import java.util.logging.Filter;
import java.util.logging.LogRecord;

public class JulApplicationFilter implements Filter {
  private final LevelAndNameFilter underlying;

  public JulApplicationFilter(LevelAndNameFilter underlying) {
    this.underlying = underlying;
  }

  @Override
  public boolean isLoggable(LogRecord record) {
    return underlying.isLoggable(record.getLevel(), record.getLoggerName());
  }
}
