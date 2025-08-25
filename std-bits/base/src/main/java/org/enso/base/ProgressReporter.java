package org.enso.base;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Raise progress steps to Logging framework. Does not check Context.safepoint as it is expected to
 * be handled by Enso.
 */
public final class ProgressReporter implements AutoCloseable {
  private static final Logger log = LoggerFactory.getLogger("Standard.Base.Logging.Progress");

  public static ProgressReporter create(String name, long count) {
    // Default step size is 1/20th of the total count.
    return createWithStep(name, count, count / 20L);
  }

  public static ProgressReporter createWithStep(String name, long count, long stepSize) {
    var result = new ProgressReporter(name, count, stepSize);
    log.trace("INIT {}:{}@{}", result.handle, "Process started", count);
    return result;
  }

  private final Object handle;
  private final String name;
  private final long count;
  private final long stepSize;
  private long step;

  private ProgressReporter(String name, long count, long stepSize) {
    this.name = name;
    this.handle =
        new Object() {
          @Override
          public String toString() {
            return name;
          }
        };
    this.count = count;
    this.stepSize = stepSize;
    this.step = stepSize;
  }

  @Override
  public String toString() {
    return name;
  }

  public void advance() {
    step--;
    if (step == 0) {
      log.trace("ADVANCE {}+{}", handle, stepSize);
      step = stepSize;
    }
  }

  @Override
  public void close() {
    log.trace("ADVANCE {}+{}", handle, count);
  }
}
