package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.job.{
  SlowUpsertVisualizationJob,
  UpsertVisualizationJob
}
import org.enso.polyglot.runtime.Runtime.Api

class SlowAttachVisualizationCmd(
  maybeRequestId: Option[Api.RequestId],
  request: Api.AttachVisualization,
  delay: Boolean
) extends AttachVisualizationCmd(maybeRequestId, request) {

  override def upsertVisualization(
    maybeRequestId: Option[Api.RequestId],
    visualizationId: Api.VisualizationId,
    expressionId: Api.ExpressionId,
    config: Api.VisualizationConfiguration
  ): UpsertVisualizationJob = {
    new SlowUpsertVisualizationJob(
      maybeRequestId,
      visualizationId,
      expressionId,
      config,
      delay
    )
  }
}
