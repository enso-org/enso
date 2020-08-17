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
  * @param sendMethodCallUpdates a flag to send all the method calls of the
  * executed frame as a value updates
  */
class ExecuteJob(
  contextId: UUID,
  stack: List[InstrumentFrame],
  updatedVisualisations: Seq[UUID],
  sendMethodCallUpdates: Boolean
) extends Job[Unit](List(contextId), true, true)
    with ProgramExecutionSupport {

  def this(exe: Executable) =
    this(
      exe.contextId,
      exe.stack.toList,
      exe.updatedVisualisations,
      exe.sendMethodCallUpdates
    )

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Unit = {
    ctx.locking.acquireContextLock(contextId)
    ctx.locking.acquireReadCompilationLock()
    ctx.executionService.getContext.getThreadManager.enter()
    try {
      val errorOrOk =
        runProgram(
          contextId,
          stack,
          updatedVisualisations,
          sendMethodCallUpdates
        )
      errorOrOk match {
        case Left(e) =>
          ctx.endpoint.sendToClient(
            Api.Response(Api.ExecutionFailed(contextId, e))
          )
        case Right(()) =>
          ctx.endpoint.sendToClient(
            Api.Response(Api.ExecutionSuccessful(contextId))
          )
      }
    } finally {
      ctx.executionService.getContext.getThreadManager.leave()
      ctx.locking.releaseReadCompilationLock()
      ctx.locking.releaseContextLock(contextId)
    }
  }

}
