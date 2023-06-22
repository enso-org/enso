package org.enso.interpreter.instrument.job

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api.{
  ContextId,
  ExpressionId,
  VisualisationId
}

import java.util.logging.Level

/** A job that detaches a visualisation.
  *
  * @param visualisationId an identifier of visualisation
  * @param expressionId an identifier of expression
  * @param contextId an execution context id
  */
class DetachVisualisationJob(
  visualisationId: VisualisationId,
  expressionId: ExpressionId,
  contextId: ContextId
) extends UniqueJob[Unit](expressionId, List(contextId), false) {

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Unit = {
    val logger        = ctx.executionService.getLogger
    val lockTimestamp = ctx.locking.acquireContextLock(contextId)
    try {
      ctx.contextManager.removeVisualisation(
        contextId,
        expressionId,
        visualisationId
      )
    } finally {
      ctx.locking.releaseContextLock(contextId)
      logger.log(
        Level.FINEST,
        s"Kept context lock [DetachVisualisationJob] for ${System.currentTimeMillis() - lockTimestamp} milliseconds"
      )
    }
  }
}
