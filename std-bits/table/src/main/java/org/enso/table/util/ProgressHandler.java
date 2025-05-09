package org.enso.table.util;

import org.graalvm.polyglot.Context;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class ProgressHandler implements AutoCloseable {
  private static final Logger log = LoggerFactory.getLogger("Standard.Base.Logging.Progress");
  private static final int PROGRESS_STEP = 5000;

  private final String name;
  private final long count;
  private long step;
  private boolean done;
  private Context context;

  private ProgressHandler(String name, long count) {
    this.name = name;
    this.count = count;
    this.step = 0;
    this.done = false;
    this.context = Context.getCurrent();
  }

  @Override
  public String toString() {
    return name;
  }

  public void advance() {
    context.safepoint();

    if (done) {
      return;
    }

    step++;
    if (step == PROGRESS_STEP) {
      log.trace("ADVANCE {}+{}", this, PROGRESS_STEP);
      step = 0;
    }
  }

  @Override
  public void close() {
    log.trace("ADVANCE {}+{}", this, count);
    this.done = true;
  }

  public static ProgressHandler init(String name, long count) {
    var result = new ProgressHandler(name, count);
    log.trace("INIT {}:{}@{}", name, result, count);
    return result;
  }
}
