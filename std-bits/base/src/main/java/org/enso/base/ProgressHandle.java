package org.enso.base;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProgressHandle {
  private static final Logger LOGGER = LoggerFactory.getLogger("Standard.Base.Logging.Progress");

  private final String name;
  private final long count;
  boolean closed = false;

  public ProgressHandle(String name, long count) {
    this.name = name;
    this.count = count;
  }

  public void close() {
    if (closed) {
      return;
    }
    closed = true;
    LOGGER.trace("ADVANCE {}+{}", this, count);
  }

  @Override
  public String toString() {
    return name;
  }
}
