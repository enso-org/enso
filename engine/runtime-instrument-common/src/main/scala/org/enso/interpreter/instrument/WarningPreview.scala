package org.enso.interpreter.instrument

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.VisualizationResult
import org.enso.interpreter.service.ExecutionService

import java.nio.charset.StandardCharsets
import java.util.concurrent.CompletableFuture

object WarningPreview {

  private[this] val METHOD = ".to_display_text"

  /** Execute preview of the provided warning value.
    *
    * @param value the warning value
    * @param ctx the runtime context
    * @return the string representation of the warning
    */
  def execute(value: AnyRef)(implicit ctx: RuntimeContext): String = {
    val visualizationExpressionFuture: CompletableFuture[AnyRef] =
      ctx.executionService.evaluateExpression(
        ctx.executionService.getContext.getBuiltins.getModule,
        METHOD
      )
    val visualizationResultFuture =
      visualizationExpressionFuture.thenCompose(visualizationExpression =>
        ctx.executionService.callFunction(
          visualizationExpression,
          value
        )
      )
    val visualizationResult =
      ExecutionService.resultOf(visualizationResultFuture)
    val bytes =
      VisualizationResult.visualizationResultToBytes(visualizationResult)
    new String(bytes, StandardCharsets.UTF_8)
  }

}
