package org.enso.interpreter.instrument.job

import java.util.UUID
import org.enso.interpreter.instrument.InstrumentFrame
import org.enso.interpreter.instrument.execution.{Executable, RuntimeContext}
import org.enso.interpreter.runtime.state.ExecutionEnvironment
import org.enso.polyglot.runtime.Runtime.Api

/** A job responsible for executing a call stack for the provided context.
  *
  * @param contextId an identifier of a context to execute
  * @param stack a call stack to execute
  * @param executionEnvironment the execution environment to use
  */
class ExecuteJob(
  contextId: UUID,
  stack: List[InstrumentFrame],
  val executionEnvironment: Option[Api.ExecutionEnvironment]
) extends Job[Unit](
      List(contextId),
      isCancellable = true,
      // TODO[MK]: make this interruptible when https://github.com/oracle/graal/issues/3590
      // is resolved
      mayInterruptIfRunning = false
    ) {

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Unit = {
    ctx.locking.acquireReadCompilationLock()
    ctx.locking.acquireContextLock(contextId)
    val context = ctx.executionService.getContext
    val originalExecutionEnvironment =
      executionEnvironment.map(_ => context.getExecutionEnvironment)
    try {
      executionEnvironment.foreach(env =>
        context.setExecutionEnvironment(ExecutionEnvironment.forName(env.name))
      )
      val outcome = ProgramExecutionSupport.runProgram(contextId, stack)
      outcome.foreach {
        case diagnostic: Api.ExecutionResult.Diagnostic =>
          ctx.endpoint.sendToClient(
            Api.Response(Api.ExecutionUpdate(contextId, Seq(diagnostic)))
          )
        case failure: Api.ExecutionResult.Failure =>
          ctx.endpoint.sendToClient(
            Api.Response(Api.ExecutionFailed(contextId, failure))
          )
      }
    } finally {
      originalExecutionEnvironment.foreach(context.setExecutionEnvironment)
      ctx.locking.releaseContextLock(contextId)
      ctx.locking.releaseReadCompilationLock()
    }
    ctx.endpoint.sendToClient(Api.Response(Api.ExecutionComplete(contextId)))
    StartBackgroundProcessingJob.startBackgroundJobs()
  }

}

object ExecuteJob {

  /** Create execute job from the executable.
    *
    * @param executable the executable to run
    * @return the new execute job
    */
  def apply(executable: Executable): ExecuteJob =
    new ExecuteJob(executable.contextId, executable.stack.toList, None)

  /** Create execute job from the context and stack.
    *
    * @param contextId the contextId to execute
    * @param stack the stack to execute
    * @return new execute job
    */
  def apply(contextId: UUID, stack: List[InstrumentFrame]): ExecuteJob =
    new ExecuteJob(contextId, stack, None)
}
