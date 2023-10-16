package org.enso.interpreter.instrument.job

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api.{
  ContextId,
  ExpressionId,
  VisualizationId
}

import java.util.logging.Level

/** A job that detaches a visualization.
  *
  * @param visualizationId an identifier of visualization
  * @param expressionId an identifier of expression
  * @param contextId an execution context id
  */
class DetachVisualizationJob(
  visualizationId: VisualizationId,
  val expressionId: ExpressionId,
  contextId: ContextId
) extends Job[Unit](List(contextId), false, false)
    with UniqueJob[Unit] {

  /** @inheritdoc */
  override def equalsTo(that: UniqueJob[_]): Boolean =
    that match {
      case that: DetachVisualizationJob =>
        this.expressionId == that.expressionId
      case _ => false
    }

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Unit = {
    val logger        = ctx.executionService.getLogger
    val lockTimestamp = ctx.locking.acquireContextLock(contextId)
    try {
      ctx.contextManager.removeVisualization(
        contextId,
        expressionId,
        visualizationId
      )
    } finally {
      ctx.locking.releaseContextLock(contextId)
      logger.log(
        Level.FINEST,
        s"Kept context lock [DetachVisualizationJob] for ${System.currentTimeMillis() - lockTimestamp} milliseconds"
      )
    }
  }
}
