package org.enso.interpeter.instrument

import org.enso.polyglot.runtime.Runtime.Api.{ExpressionId, VisualisationId}

/**
  * A mutable holder of all visualisations attached to an execution context.
  */
class VisualisationHolder() {

  private var visualisationMap: Map[ExpressionId, List[Visualisation]] =
    Map.empty.withDefaultValue(List.empty)

  /**
    * Upserts a visualisation.
    *
    * @param visualisation the visualisation to upsert
    */
  def upsert(visualisation: Visualisation): Unit = {
    val visualisations = visualisationMap(visualisation.expressionId)
    val removed        = visualisations.filterNot(_.id == visualisation.id)
    visualisationMap += (visualisation.expressionId -> (visualisation :: removed))
  }

  /**
    * Removes a visualisation from the holder.
    *
    * @param visualisationId the visualisation identifier
    * @param expressionId the id of expression that the visualisation is
    *                     attached to
    */
  def remove(
    visualisationId: VisualisationId,
    expressionId: ExpressionId
  ): Unit = {
    val visualisations = visualisationMap(expressionId)
    val removed        = visualisations.filterNot(_.id == visualisationId)
    visualisationMap += (expressionId -> removed)
  }

  /**
    * Finds all visualisations attached to an expression.
    *
    * @param expressionId the unique identifier of the expression
    * @return a list of matching visualisation
    */
  def find(expressionId: ExpressionId): List[Visualisation] =
    visualisationMap(expressionId)

  /**
    * Returns a visualisation with the provided id.
    *
    * @param visualisationId the identifier of visualisation
    * @return an option with visualisation
    */
  def getById(visualisationId: VisualisationId): Option[Visualisation] =
    visualisationMap.values.flatten.find(_.id == visualisationId)

}

object VisualisationHolder {

  /**
    * Returns an empty holder.
    */
  def empty = new VisualisationHolder()

}
