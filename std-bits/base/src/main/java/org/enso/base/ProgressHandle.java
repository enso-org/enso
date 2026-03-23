package org.enso.base;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProgressHandle {
  private static final Logger LOGGER = LoggerFactory.getLogger("Standard.Base.Logging.Progress");

  private final String name;
  private final long count;
  private final long then;
  private Long took;

  public ProgressHandle(String name, long count) {
    this.name = name;
    this.count = count;
    this.then = System.currentTimeMillis();
  }

  public void close() {
    if (took != null) {
      return;
    }
    took = System.currentTimeMillis() - then;
    LOGGER.trace("ADVANCE {}+{}~{}ms", this, count, took);
  }

  @Override
  public String toString() {
    return name;
  }
}
