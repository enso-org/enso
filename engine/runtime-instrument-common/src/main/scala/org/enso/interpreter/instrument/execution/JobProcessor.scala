package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.job.{BackgroundJob, Job}

import scala.concurrent.Future

/** Defines a uniform interface to execute job.
  */
trait JobProcessor {

  /** Runs a job with the provided context.
    *
    * @param job a job to execute
    * @return the future result of an asynchronous computation
    */
  def run[A](job: Job[A]): Future[A]

  /** Runs a job with the provided context in the background.
    *
    * @param job a job to execute
    * @return the future result of an asynchronous computation
    */
  def runBackground[A](job: BackgroundJob[A]): Unit

  /** Stops the job processor. */
  def stop(): Unit

}
