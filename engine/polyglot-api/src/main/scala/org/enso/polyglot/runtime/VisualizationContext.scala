package org.enso.polyglot.runtime

import com.github.plokhotnyuk.jsoniter_scala.macros.named
import org.enso.polyglot.runtime.Runtime.Api.{
  ContextId,
  ExpressionId,
  VisualizationId
}

/** Represents a visualization context.
  *
  * @param visualizationId a visualization identifier
  * @param contextId a context identifier
  * @param expressionId an expression identifier
  */
@named("visualizationContext")
case class VisualizationContext(
  visualizationId: VisualizationId,
  contextId: ContextId,
  expressionId: ExpressionId
)
