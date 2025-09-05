package org.enso.interpreter.instrument.job

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api.{
  ContextId,
  ExpressionId,
  VisualizationId
}
import org.slf4j.{Logger, LoggerFactory}

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
  override def runImpl(implicit ctx: RuntimeContext): Unit = {
    ctx.locking.withReadContextLock(
      ctx.locking.getOrCreateContextLock(contextId),
      this.getClass,
      () => {
        val stack =
          ctx.contextManager.getStack(contextId)
        val runtimeCache = stack.headOption
          .flatMap(frame => Option(frame.cache))
        val result = runtimeCache.exists(cache =>
          cache.deregisterAction(expressionId, visualizationId)
        )
        if (!result) {
          DetachVisualizationJob.logger.warn(
            "Failed to detach visualization {} - unknown visualization/expression",
            visualizationId
          )
        }
      }
    )
  }
}

object DetachVisualizationJob {
  private lazy val logger: Logger =
    LoggerFactory.getLogger(classOf[DetachVisualizationJob])
}
