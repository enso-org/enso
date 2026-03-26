package org.enso.base;

import org.slf4j.Logger;

public final class ProgressHandle {
  private final Logger logger;
  private final String name;
  private final String label;
  private final long count;
  private final long then;
  private boolean closed;

  public ProgressHandle(Logger logger, String name, long count, String label) {
    this.logger = logger;
    this.name = name;
    this.count = count;
    this.label = label;
    this.then = System.currentTimeMillis();
  }

  public void close() {
    if (closed) {
      return;
    }
    closed = true;
    var took = System.currentTimeMillis() - then;
    logger.debug("ADVANCE {}+{}~{}ms:{}", this, count, took, label);
  }

  @Override
  public String toString() {
    return name;
  }
}
