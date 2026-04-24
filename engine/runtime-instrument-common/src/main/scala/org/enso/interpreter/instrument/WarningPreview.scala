package org.enso.interpreter.instrument

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.VisualizationResult

import java.nio.charset.StandardCharsets
import java.util.concurrent.CompletionStage

object WarningPreview {

  private[this] val METHOD = ".to_display_text"

  /** Execute preview of the provided warning value.
    *
    * @param value the warning value
    * @param ctx the runtime context
    * @return the string representation of the warning
    */
  def execute(
    value: AnyRef
  )(implicit ctx: RuntimeContext): CompletionStage[String] = {
    val textExtensions =
      ctx.executionService.getContext.getBuiltins.textExtensions.getDefinitionScope.getModule
    val visualizationExpressionFuture: CompletionStage[AnyRef] =
      ctx.executionService.evaluateExpression(
        textExtensions,
        METHOD,
        "warning preview"
      )
    val visualizationResultFuture =
      visualizationExpressionFuture.thenCompose(visualizationExpression =>
        ctx.executionService.callFunction(
          visualizationExpression,
          value
        )
      )

    visualizationResultFuture.thenApply(visualizationResult => {
      val bytes =
        VisualizationResult.visualizationResultToBytes(visualizationResult)
      new String(bytes, StandardCharsets.UTF_8)
    })
  }

}
