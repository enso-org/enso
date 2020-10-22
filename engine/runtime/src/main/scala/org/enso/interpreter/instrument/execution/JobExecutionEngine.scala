package org.enso.interpreter.instrument.execution

import java.util.UUID
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, Executors}
import java.util.logging.Level

import org.enso.interpreter.instrument.InterpreterContext
import org.enso.interpreter.instrument.job.Job
import org.enso.polyglot.RuntimeServerInfo

import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

/** This component schedules the execution of jobs. It keep a queue of
  * pending jobs and activates job execution in FIFO order.
  *
  * @param interpreterContext suppliers of services that provide interpreter
  *                           specific functionality
  * @param locking locking capability for runtime
  */
class JobExecutionEngine(
  interpreterContext: InterpreterContext,
  locking: Locking
) extends JobProcessor
    with JobControlPlane {

  private val runningJobsRef =
    new AtomicReference[Vector[RunningJob]](Vector.empty)

  private val context = interpreterContext.executionService.getContext

  private val jobParallelism =
    interpreterContext.executionService.getContext.getEnvironment.getOptions
      .get(RuntimeServerInfo.JOB_PARALLELISM_KEY)
      .intValue()

  val jobExecutor = Executors.newFixedThreadPool(
    jobParallelism,
    new TruffleThreadFactory(context, "job-pool")
  )

  private val runtimeContext =
    RuntimeContext(
      executionService = interpreterContext.executionService,
      contextManager   = interpreterContext.contextManager,
      endpoint         = interpreterContext.endpoint,
      truffleContext   = interpreterContext.truffleContext,
      jobProcessor     = this,
      jobControlPlane  = this,
      locking          = locking
    )

  /** @inheritdoc */
  override def run[A](job: Job[A]): Future[A] = {
    val jobId   = UUID.randomUUID()
    val promise = Promise[A]()
    val future = jobExecutor.submit[Unit](new Callable[Unit] {
      override def call(): Unit = {
        val logger = runtimeContext.executionService.getLogger
        logger.log(Level.FINE, s"Executing job: $job...")
        try {
          val result = job.run(runtimeContext)
          logger.log(Level.FINE, s"Job $job finished.")
          promise.success(result)
        } catch {
          case NonFatal(ex) => promise.failure(ex)
        } finally {
          runningJobsRef.updateAndGet(_.filterNot(_.id == jobId))
        }
      }
    })
    val runningJob = RunningJob(jobId, job, future)

    runningJobsRef.updateAndGet(_ :+ runningJob)

    promise.future
  }

  /** @inheritdoc */
  override def abortAllJobs(): Unit = {
    val allJobs         = runningJobsRef.get()
    val cancellableJobs = allJobs.filter(_.job.isCancellable)
    cancellableJobs.foreach { runningJob =>
      runningJob.future.cancel(runningJob.job.mayInterruptIfRunning)
    }
    runtimeContext.executionService.getContext.getThreadManager
      .checkInterrupts()
  }

  /** @inheritdoc */
  override def abortJobs(contextId: UUID): Unit = {
    val allJobs     = runningJobsRef.get()
    val contextJobs = allJobs.filter(_.job.contextIds.contains(contextId))
    contextJobs.foreach { runningJob =>
      if (runningJob.job.isCancellable) {
        runningJob.future.cancel(runningJob.job.mayInterruptIfRunning)
      }
    }
    runtimeContext.executionService.getContext.getThreadManager
      .checkInterrupts()
  }

  /** @inheritdoc */
  override def stop(): Unit = {
    val allJobs = runningJobsRef.get()
    allJobs.foreach(_.future.cancel(true))
    runtimeContext.executionService.getContext.getThreadManager
      .checkInterrupts()
    jobExecutor.shutdownNow()
  }

}
