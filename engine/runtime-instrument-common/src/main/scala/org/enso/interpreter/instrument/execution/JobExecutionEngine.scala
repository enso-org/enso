package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.InterpreterContext
import org.enso.interpreter.instrument.job.{BackgroundJob, Job, UniqueJob}

import java.util
import java.util.{Collections, UUID}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.ExecutorService
import java.util.logging.Level

import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

/** This component schedules the execution of jobs. It keeps a queue of
  * pending jobs and activates job execution in FIFO order.
  *
  * @param interpreterContext suppliers of services that provide interpreter
  * specific functionality
  * @param executionState a state of the runtime
  * @param locking locking capability for runtime
  */
final class JobExecutionEngine(
  interpreterContext: InterpreterContext,
  executionState: ExecutionState,
  locking: Locking
) extends JobProcessor
    with JobControlPlane {

  private val runningJobsRef =
    new AtomicReference[Vector[RunningJob]](Vector.empty)

  private val backgroundJobsRef =
    new AtomicReference[Vector[RunningJob]](Vector.empty)

  private val context = interpreterContext.executionService.getContext

  private val jobParallelism = context.getJobParallelism

  private var isBackgroundJobsStarted = false

  private val delayedBackgroundJobsQueue =
    new util.ArrayList[BackgroundJob[_]](4096)

  val jobExecutor: ExecutorService =
    context.newFixedThreadPool(jobParallelism, "job-pool", false)

  private val backgroundJobExecutor: ExecutorService =
    context.newFixedThreadPool(1, "background-job-pool", false)

  private val runtimeContext =
    RuntimeContext(
      executionService = interpreterContext.executionService,
      contextManager   = interpreterContext.contextManager,
      endpoint         = interpreterContext.endpoint,
      truffleContext   = interpreterContext.truffleContext,
      jobProcessor     = this,
      jobControlPlane  = this,
      locking          = locking,
      state            = executionState
    )

  /** @inheritdoc */
  override def runBackground[A](job: BackgroundJob[A]): Unit =
    synchronized {
      if (isBackgroundJobsStarted) {
        runInternal(job, backgroundJobExecutor, backgroundJobsRef)
      } else {
        delayedBackgroundJobsQueue.add(job)
      }
    }

  /** @inheritdoc */
  override def run[A](job: Job[A]): Future[A] = {
    cancelDuplicateJobs(job)
    runInternal(job, jobExecutor, runningJobsRef)
  }

  private def cancelDuplicateJobs[A](job: Job[A]): Unit = {
    job match {
      case job: UniqueJob[_] =>
        val allJobs =
          runningJobsRef.updateAndGet(_.filterNot(_.future.isCancelled))
        allJobs.foreach { runningJob =>
          runningJob.job match {
            case jobRef: UniqueJob[_]
                if jobRef.getClass == job.getClass && jobRef.key == job.key =>
              runtimeContext.executionService.getLogger
                .log(Level.FINEST, s"Cancelling duplicate job [$jobRef].")
              runningJob.future.cancel(jobRef.mayInterruptIfRunning)
            case _ =>
          }
        }
      case _ =>
    }
  }

  private def runInternal[A](
    job: Job[A],
    executorService: ExecutorService,
    runningJobsRef: AtomicReference[Vector[RunningJob]]
  ): Future[A] = {
    val jobId   = UUID.randomUUID()
    val promise = Promise[A]()
    val logger  = runtimeContext.executionService.getLogger
    logger.log(Level.FINE, s"Submitting job: $job...")
    val future = executorService.submit(() => {
      logger.log(Level.FINE, s"Executing job: $job...")
      val before = System.currentTimeMillis()
      try {
        val result = job.run(runtimeContext)
        val took   = System.currentTimeMillis() - before
        logger.log(Level.FINE, s"Job $job finished in $took ms.")
        promise.success(result)
      } catch {
        case NonFatal(ex) =>
          logger.log(Level.SEVERE, s"Error executing $job", ex)
          promise.failure(ex)
        case err: Throwable =>
          logger.log(Level.SEVERE, s"Error executing $job", err)
          throw err
      } finally {
        runningJobsRef.updateAndGet(_.filterNot(_.id == jobId))
      }
    })
    val runningJob = RunningJob(jobId, job, future)

    runningJobsRef.updateAndGet(_ :+ runningJob)

    promise.future
  }

  /** @inheritdoc */
  override def abortAllJobs(): Unit =
    abortAllExcept()

  /** @inheritdoc */
  override def abortAllExcept(ignoredJobs: Class[_ <: Job[_]]*): Unit = {
    val allJobs = runningJobsRef.updateAndGet(_.filterNot(_.future.isCancelled))
    val cancellableJobs = allJobs
      .filter { runningJob =>
        runningJob.job.isCancellable &&
        !ignoredJobs.contains(runningJob.job.getClass)
      }
    cancellableJobs.foreach { runningJob =>
      runningJob.future.cancel(runningJob.job.mayInterruptIfRunning)
    }
    runtimeContext.executionService.getContext.getThreadManager
      .interruptThreads()
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
      .interruptThreads()
  }

  /** @inheritdoc */
  override def startBackgroundJobs(): Boolean =
    synchronized {
      val result = !isBackgroundJobsStarted
      isBackgroundJobsStarted = true
      submitBackgroundJobsOrdered()
      result
    }

  /** @inheritdoc */
  override def stop(): Unit = {
    val allJobs = runningJobsRef.get()
    allJobs.foreach(_.future.cancel(true))
    runtimeContext.executionService.getContext.getThreadManager
      .interruptThreads()
    jobExecutor.shutdownNow()
    backgroundJobExecutor.shutdownNow()
  }

  /** Submit background jobs preserving the stable order. */
  private def submitBackgroundJobsOrdered(): Unit = {
    Collections.sort(delayedBackgroundJobsQueue)
    delayedBackgroundJobsQueue.forEach(job => runBackground(job))
    delayedBackgroundJobsQueue.clear()
  }
}
