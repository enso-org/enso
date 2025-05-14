package org.enso.table.util;

import org.graalvm.polyglot.Context;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class ProgressHandler implements AutoCloseable {
  private static final Logger log = LoggerFactory.getLogger("Standard.Base.Logging.Progress");
  private static final long PROGRESS_STEP = 50000;

  private final Object handle;
  private final String name;
  private final long count;
  private final Context context;
  private long step;

  private ProgressHandler(String name, long count) {
    this.name = name;
    this.handle =
        new Object() {
          @Override
          public String toString() {
            return name;
          }
        };
    this.count = count;
    this.context = Context.getCurrent();
    this.step = PROGRESS_STEP;
  }

  @Override
  public String toString() {
    return name;
  }

  public void advance() {
    step--;
    if (step == 0) {
      context.safepoint();
      log.trace("ADVANCE {}+{}", handle, PROGRESS_STEP);
      step = PROGRESS_STEP;
    }
  }

  @Override
  public void close() {
    log.trace("ADVANCE {}+{}", handle, count);
  }

  public static ProgressHandler init(String name, long count) {
    var result = new ProgressHandler(name, count);
    log.trace("INIT {}:{}@{}", result.handle, "Process started", count);
    return result;
  }
}
