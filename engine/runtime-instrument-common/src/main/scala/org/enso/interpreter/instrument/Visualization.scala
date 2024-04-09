package org.enso.interpreter.instrument

import org.enso.interpreter.runtime.Module
import org.enso.polyglot.runtime.Runtime.Api.{
  ContextId,
  ExpressionId,
  VisualizationConfiguration,
  VisualizationId
}

/** An object containing visualization data.
  *
  * @param id the unique identifier of visualization
  * @param expressionId the identifier of expression that the visualization is
  *                     attached to
  * @param callback the callable expression used to generate visualization data
  */
case class Visualization(
  id: VisualizationId,
  expressionId: ExpressionId,
  cache: RuntimeCache,
  module: Module,
  config: VisualizationConfiguration,
  visualizationExpressionId: Option[ExpressionId],
  callback: AnyRef,
  arguments: Vector[AnyRef]
)

/** An expression that will be executed in the local scope.
  *
  * @param id the unique identifier of visualization
  * @param expressionId the identifier of expression that provides the execution scope
  * @param executionContextId the identifier of the execution context
  * @param expression the expression to execute
  */
case class OneshotExpression(
  id: VisualizationId,
  expressionId: ExpressionId,
  executionContextId: ContextId,
  expression: String
)
