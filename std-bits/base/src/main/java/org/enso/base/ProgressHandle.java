package org.enso.base;

import org.slf4j.Logger;

public final class ProgressHandle {
  private final Logger log;
  private final String name;
  private final long count;
  private final long then;
  private Long took;

  public ProgressHandle(Logger log, String name, long count) {
    this.log = log;
    this.name = name;
    this.count = count;
    this.then = System.currentTimeMillis();
  }

  public void close() {
    if (took != null) {
      return;
    }
    took = System.currentTimeMillis() - then;
    log.trace("ADVANCE {}+{}~{}ms", this, count, took);
  }

  @Override
  public String toString() {
    return name;
  }
}
