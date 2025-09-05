package org.enso.base;

import org.graalvm.polyglot.Context;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Raise progress steps to Logging framework. Does not check Context.safepoint as it is expected to
 * be handled by Enso.
 */
public final class ProgressReporter implements AutoCloseable {
  private static final Logger LOGGER = LoggerFactory.getLogger("Standard.Base.Logging.Progress");

  public static ProgressReporter create(String name, long count) {
    // Default step size is 1/20th of the total count (or 1 if smaller).
    return createWithStep(name, count, Math.max(1, count / 20L));
  }

  public static ProgressReporter createWithStep(String name, long count, long stepSize) {
    var result = new ProgressReporter(name, count, stepSize);
    LOGGER.trace("INIT {}:{}@{}", result.handle, "Process started", count);
    return result;
  }

  private final Object handle;
  private final String name;
  private final long count;
  private final long stepSize;
  private final Context context;
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
    this.context = Context.getCurrent();
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
      context.safepoint();
      LOGGER.trace("ADVANCE {}+{}", handle, stepSize);
      step = stepSize;
    }
  }

  @Override
  public void close() {
    LOGGER.trace("ADVANCE {}+{}", handle, count);
  }
}
