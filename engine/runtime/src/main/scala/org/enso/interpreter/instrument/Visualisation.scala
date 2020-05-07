package org.enso.interpreter.instrument

import org.enso.polyglot.runtime.Runtime.Api.{ExpressionId, VisualisationId}

/**
  * An object containing visualisation data.
  *
  * @param id the unique identifier of visualisation
  * @param expressionId the identifier of expression that the visualisation is
  *                     attached to
  * @param callback the callable expression used to generate visualisation data
  */
case class Visualisation(
  id: VisualisationId,
  expressionId: ExpressionId,
  callback: AnyRef
)
