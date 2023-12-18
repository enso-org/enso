package org.enso.interpreter.instrument.job;

import java.util.Comparator;
import scala.collection.immutable.List$;

/** The job that runs in the background. */
public abstract class BackgroundJob<A> extends Job<A> {

  private final int priority;

  /** Comparator defining the order of jobs in the background jobs queue. */
  public static final Comparator<BackgroundJob<?>> BACKGROUND_JOBS_QUEUE_ORDER =
      Comparator.comparingInt(BackgroundJob::getPriority);

  /**
   * Create a background job with priority.
   *
   * @param priority the job priority. Lower number indicates higher priority.
   */
  public BackgroundJob(int priority) {
    super(List$.MODULE$.empty(), true, false);
    this.priority = priority;
  }

  /**
   * @return the job priority.
   */
  public int getPriority() {
    return priority;
  }
}
