package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.job.{
  SlowUpsertVisualizationJob,
  UpsertVisualizationJob
}
import org.enso.polyglot.runtime.Runtime.Api
import org.slf4j.LoggerFactory

class SlowAttachVisualizationCmd(
  maybeRequestId: Option[Api.RequestId],
  request: Api.AttachVisualization,
  delay: Boolean
) extends AttachVisualizationCmd(maybeRequestId, request) {

  private val logger =
    LoggerFactory.getLogger(classOf[SlowAttachVisualizationCmd])

  override def upsertVisualization(
    maybeRequestId: Option[Api.RequestId],
    visualizationId: Api.VisualizationId,
    expressionId: Api.ExpressionId,
    config: Api.VisualizationConfiguration
  ): UpsertVisualizationJob = {

    logger.info("Delaying upsert for {}: {}", request.visualizationId, delay)
    new SlowUpsertVisualizationJob(
      maybeRequestId,
      visualizationId,
      expressionId,
      config,
      delay
    )
  }
}
