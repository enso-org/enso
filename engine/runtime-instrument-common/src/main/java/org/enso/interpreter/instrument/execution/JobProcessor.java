package org.enso.interpreter.instrument.execution;

import org.enso.interpreter.instrument.job.BackgroundJob;
import org.enso.interpreter.instrument.job.Job;

public interface JobProcessor {

  /**
   * Runs a job with the provided context.
   *
   * @param job a job to execute
   * @return the future result of an asynchronous computation
   */
  <T> scala.concurrent.Future<T> run(Job<T> job);

  /**
   * Runs a job with the provided context in the background.
   *
   * @param job a job to execute
   * @return the future result of an asynchronous computation
   */
  <T> void runBackground(BackgroundJob<T> job);

  /** Stops the job processor. */
  void stop();
}
