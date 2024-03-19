package org.enso.interpreter.instrument.execution;

import java.util.UUID;
import org.enso.interpreter.instrument.job.Job;

/** Controls running jobs. */
public interface JobControlPlane {

  /** Aborts all interruptible jobs. */
  void abortAllJobs();

  /**
   * Abort all jobs except the ignored jobs.
   *
   * @param ignoredJobs the list of jobs to keep in the execution queue
   */
  @SuppressWarnings("unchecked")
  void abortAllExcept(Class<? extends Job<?>>... ignoredJobs);

  /**
   * Aborts jobs that relates to the specified execution context.
   *
   * @param contextId an identifier of a context
   * @param classOf abort jobs of a given class only. If empty all jobs for the given context are
   *     aborted
   */
  @SuppressWarnings("unchecked")
  void abortJobs(UUID contextId, Class<? extends Job<?>>... classOf);

  /**
   * Abort provided background jobs.
   *
   * @param toAbort the list of jobs to abort
   */
  @SuppressWarnings("unchecked")
  void abortBackgroundJobs(Class<? extends Job<?>>... toAbort);

  /**
   * Starts background jobs processing.
   *
   * @return `true` if the background jobs were started and `false` if they are already running.
   */
  boolean startBackgroundJobs();

  /**
   * Stops background jobs processing.
   *
   * @return `true` if the call stopped background job, `false` if they are already stopped.
   */
  boolean stopBackgroundJobs();

  /** Finds the first in-progress job satisfying the `filter` condition */
  <T> scala.Option<T> jobInProgress(scala.PartialFunction<Job<?>, scala.Option<T>> filter);
}
