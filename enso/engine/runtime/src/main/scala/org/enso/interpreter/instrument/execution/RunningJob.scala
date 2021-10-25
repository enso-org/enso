package org.enso.interpreter.instrument.execution

import java.util.UUID
import java.util.concurrent.Future

import org.enso.interpreter.instrument.job.Job

/** Represents a running job.
  *
  * @param id a job id
  * @param job a job
  * @param future represents the result of an asynchronous computation
  */
case class RunningJob(id: UUID, job: Job[_], future: Future[_])
