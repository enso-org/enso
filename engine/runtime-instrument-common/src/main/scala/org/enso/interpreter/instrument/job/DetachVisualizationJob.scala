package org.enso.interpreter.instrument.job

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api.{
  ContextId,
  ExpressionId,
  VisualizationId
}

/** A job that detaches a visualization.
  *
  * @param visualizationId an identifier of visualization
  * @param expressionId an identifier of expression
  * @param contextId an execution context id
  */
class DetachVisualizationJob(
  val visualizationId: VisualizationId,
  val expressionId: ExpressionId,
  val contextId: ContextId
) extends Job[Unit](List(contextId), false, false)
    with UniqueJob[Unit] {

  /** @inheritdoc */
  override def equalsTo(that: UniqueJob[_]): Boolean =
    that match {
      case that: DetachVisualizationJob =>
        this.visualizationId == that.visualizationId &&
        this.expressionId == that.expressionId &&
        this.contextId == that.contextId
      case _ => false
    }

  /** @inheritdoc */
  override def runImpl(implicit ctx: RuntimeContext): Unit = {
    ctx.locking.withReadContextLock(
      ctx.locking.getOrCreateContextLock(contextId),
      this.getClass,
      () => {
        val holder = ctx.contextManager.getVisualizationHolder(contextId)

        holder.removeUnevaluated(visualizationId, expressionId)

        ctx.contextManager.removeVisualization(
          contextId,
          expressionId,
          visualizationId
        )
      }
    )
  }
}
