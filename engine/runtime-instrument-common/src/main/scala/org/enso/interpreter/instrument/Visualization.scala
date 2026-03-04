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
  * @param parentExpressionId non-empty id of the cached expression if visualization attached to a subexpression
  * @param callback the callable expression used to generate visualization data
  */
case class Visualization(
  id: VisualizationId,
  expressionId: ExpressionId,
  parentExpressionId: Option[ExpressionId],
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

/** Represents a visualization request that couldn't be evaluated immediately
  * because locks were unavailable. Will be "promoted" to a Visualization
  * after successful evaluation.
  *
  * @param id the unique identifier of visualization
  * @param expressionId the identifier of expression the visualization is attached to
  * @param parentExpressionId non-empty id of the cached expression if visualization attached to a subexpression
  * @param contextId the execution context id
  * @param config the visualization configuration
  */
case class UnevaluatedVisualization(
  id: VisualizationId,
  expressionId: ExpressionId,
  parentExpressionId: Option[ExpressionId],
  contextId: ContextId,
  config: VisualizationConfiguration
)
