package org.enso.interpreter.instrument.job

import java.util.UUID

import org.enso.interpreter.instrument.InstrumentFrame
import org.enso.interpreter.instrument.execution.{Executable, RuntimeContext}
import org.enso.polyglot.runtime.Runtime.Api

/**
  * A job responsible for executing a call stack for the provided context.
  *
  * @param contextId an identifier of a context to execute
  * @param stack a call stack to execute
  * @param updatedVisualisations a list of updated visualisations
  */
class ExecuteJob(
  contextId: UUID,
  stack: List[InstrumentFrame],
  updatedVisualisations: Seq[UUID]
) extends Job[Unit](List(contextId), true, true)
    with ProgramExecutionSupport {

  def this(exe: Executable) =
    this(exe.contextId, exe.stack.toList, exe.updatedVisualisations)

  /** @inheritdoc **/
  override def run(implicit ctx: RuntimeContext): Unit = {
    ctx.locking.acquireContextLock(contextId)
    ctx.locking.acquireReadCompilationLock()
    ctx.executionService.getContext.getThreadManager.enter()
    try {
      val errorOrOk = runProgram(contextId, stack, updatedVisualisations)
      errorOrOk match {
        case Left(e) =>
          ctx.endpoint.sendToClient(
            Api.Response(None, Api.ExecutionFailed(contextId, e))
          )

        case Right(()) => //nop
      }
    } finally {
      ctx.executionService.getContext.getThreadManager.leave()
      ctx.locking.releaseReadCompilationLock()
      ctx.locking.releaseContextLock(contextId)
    }
  }

}
