package org.enso.interpreter.instrument.job

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import java.util.UUID
import org.enso.interpreter.instrument.InstrumentFrame
import org.enso.interpreter.instrument.execution.{Executable, RuntimeContext}
import org.enso.interpreter.runtime.state.ExecutionEnvironment
import org.enso.polyglot.runtime.Runtime.Api

import java.util.concurrent.ExecutionException

/** A job responsible for executing a call stack for the provided context.
  *
  * @param contextId an identifier of a context to execute
  * @param stack a call stack to execute
  * @param executionEnvironment the execution environment to use
  * @param visualizationTriggered the UUID of an expression that triggered this execution when executing an expression
  */
class ExecuteJob(
  contextId: UUID,
  stack: List[InstrumentFrame],
  val executionEnvironment: Option[Api.ExecutionEnvironment],
  triggerContext: String,
  val visualizationTriggered: Option[UUID] = None
) extends Job[Unit](
      List(contextId),
      isCancellable = executionEnvironment.forall(ee =>
        ee.name != Api.ExecutionEnvironment.Live().name
      ),
      // Interruptions may turn out to be problematic in enterprise edition of GraalVM
      // until https://github.com/oracle/graal/issues/3590 is resolved
      mayInterruptIfRunning = executionEnvironment.forall(ee =>
        ee.name != Api.ExecutionEnvironment.Live().name
      )
    ) {

  private var _threadName: String            = "<unknown>"
  @volatile private var _hasStarted: Boolean = false
  private var _jobId: UUID                   = _

  override def threadNameExecutingJob(): String = _threadName

  override def hasStarted(): Boolean = _hasStarted

  override def setJobId(id: UUID): Unit = {
    _jobId = id
  }

  /** @inheritdoc */
  override def runImpl(implicit ctx: RuntimeContext): Unit = {
    _hasStarted = true
    _threadName = Thread.currentThread().getName
    try {
      ExecuteJob.logger.debug(
        "Starting ExecuteJob[{}, trigger={}]",
        _jobId,
        triggerContext
      )
      execute
    } catch {
      case t: Throwable =>
        ExecuteJob.logger.error("Failed to execute", t)
        val errorMsg = if (t.getMessage == null) {
          if (t.getCause == null) {
            t.getClass.getSimpleName
          } else {
            val cause = t.getCause
            if (cause.getMessage == null) {
              cause.getClass.getSimpleName
            } else {
              cause.getMessage
            }
          }
        } else t.getMessage

        ctx.endpoint.sendToClient(
          Api.Response(
            Api.ExecutionFailed(
              contextId,
              Api.ExecutionResult.Failure(errorMsg, None)
            )
          )
        )
    } finally {
      ExecuteJob.logger.trace(
        "Finished ExecuteJob[{}]",
        _jobId
      )
    }
  }

  private def execute(implicit ctx: RuntimeContext): Unit = {
    ctx.state.executionHooks.run()

    ctx.locking.withReadContextLock(
      ctx.locking.getOrCreateContextLock(contextId),
      this.getClass,
      () =>
        ctx.locking.withReadCompilationLock(
          this.getClass,
          () =>
            try {
              val originalExecutionEnvironment = executionEnvironment.map(env =>
                ctx.executionService
                  .setExecutionInstrument(
                    ExecutionEnvironment.forName(env.name)
                  )
                  .toCompletableFuture
                  .get()
              )
              val outcome =
                try ProgramExecutionSupport.runProgram(contextId, stack)
                finally {
                  originalExecutionEnvironment.foreach(original =>
                    ctx.executionService
                      .setExecutionInstrument(original)
                      .toCompletableFuture
                      .get()
                  )
                }
              outcome match {
                case Some(diagnostic: Api.ExecutionResult.Diagnostic) =>
                  if (diagnostic.isError) {
                    ctx.endpoint.sendToClient(
                      Api.Response(Api.ExecutionFailed(contextId, diagnostic))
                    )
                  } else {
                    ctx.endpoint.sendToClient(
                      Api.Response(
                        Api.ExecutionUpdate(contextId, Seq(diagnostic))
                      )
                    )
                    ctx.endpoint.sendToClient(
                      Api.Response(Api.ExecutionComplete(contextId))
                    )
                  }
                case Some(failure: Api.ExecutionResult.Failure) =>
                  ctx.endpoint.sendToClient(
                    Api.Response(Api.ExecutionFailed(contextId, failure))
                  )
                case None =>
                  ctx.endpoint.sendToClient(
                    Api.Response(Api.ExecutionComplete(contextId))
                  )
              }
            } catch {
              case e: ExecutionException =>
                ctx.endpoint.sendToClient(
                  Api.Response(
                    Api.ExecutionFailed(
                      contextId,
                      Api.ExecutionResult.Failure(e.getMessage, None)
                    )
                  )
                )
                throw e;
            }
        )
    )
  }

  override def toString(): String = {
    s"ExecuteJob(contextId=$contextId, jobId=${_jobId}, triggeredByVisualization=${visualizationTriggered})"
  }

}

object ExecuteJob {
  final private lazy val logger: Logger =
    LoggerFactory.getLogger(classOf[ExecuteJob])

  /** Create execute job from the executable.
    *
    * @param executable the executable to run
    * @param visualizationTriggered the UUID of an expression that triggered this execution when executing an expression, empty otherwise
    * @param triggerContext human-readable explanation for execution job
    * @return the new execute job
    */
  def apply(
    executable: Executable,
    triggerContext: String,
    visualizationTriggered: Option[UUID] = None
  ): ExecuteJob =
    new ExecuteJob(
      executable.contextId,
      executable.stack.toList,
      None,
      triggerContext,
      visualizationTriggered
    )

  /** Create execute job from the context and stack.
    *
    * @param contextId the contextId to execute
    * @param stack the stack to execute
    * @param triggerContext human-readable explanation for execution job
    * @return new execute job
    */
  def apply(
    contextId: UUID,
    stack: List[InstrumentFrame],
    triggerContext: String
  ): ExecuteJob =
    new ExecuteJob(contextId, stack, None, triggerContext)
}
