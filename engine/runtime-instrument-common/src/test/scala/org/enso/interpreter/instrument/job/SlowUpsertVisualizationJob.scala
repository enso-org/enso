package org.enso.interpreter.instrument.job

import org.enso.interpreter.instrument.execution.{Executable, RuntimeContext}
import org.enso.polyglot.runtime.Runtime.Api

import scala.annotation.unused

class SlowUpsertVisualizationJob(
  @unused requestId: Option[Api.RequestId],
  visualizationId: Api.VisualizationId,
  expressionId: Api.ExpressionId,
  config: Api.VisualizationConfiguration,
  delay: Boolean
) extends UpsertVisualizationJob(
      requestId,
      visualizationId,
      expressionId,
      config
    ) {

  override val isCancellable: Boolean         = true
  override val mayInterruptIfRunning: Boolean = true

  override def run(implicit ctx: RuntimeContext): Option[Executable] = {
    if (
      ctx.executionService.getContext.isRandomDelayedCommandExecution && delay
    ) {
      Thread.sleep(1000)
    }

    super.run(ctx)
  }
}
