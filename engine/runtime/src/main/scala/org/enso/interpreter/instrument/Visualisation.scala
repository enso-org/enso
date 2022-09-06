package org.enso.interpreter.instrument

import org.enso.interpreter.runtime.Module
import org.enso.polyglot.runtime.Runtime.Api.{
  ExpressionId,
  VisualisationConfiguration,
  VisualisationId
}

/** An object containing visualisation data.
  *
  * @param id the unique identifier of visualisation
  * @param expressionId the identifier of expression that the visualisation is
  *                     attached to
  * @param callback the callable expression used to generate visualisation data
  */
case class Visualisation(
  id: VisualisationId,
  expressionId: ExpressionId,
  cache: RuntimeCache,
  module: Module,
  config: VisualisationConfiguration,
  visualisationExpressionId: Option[ExpressionId],
  callback: AnyRef,
  arguments: Vector[AnyRef]
)
