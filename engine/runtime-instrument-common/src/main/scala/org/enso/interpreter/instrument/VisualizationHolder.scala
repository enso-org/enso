package org.enso.interpreter.instrument

import org.enso.pkg.QualifiedName
import org.enso.polyglot.runtime.Runtime.Api.{ExpressionId, VisualizationId}

import scala.collection.mutable

/** A mutable holder of all visualizations attached to an execution context.
  */
class VisualizationHolder {

  private var oneshotExpression: OneshotExpression = _

  private val visualizationMap: mutable.Map[ExpressionId, List[Visualization]] =
    mutable.Map.empty.withDefaultValue(List.empty)

  /** Upserts a visualization.
    *
    * @param visualization the visualization to upsert
    * @param specificId UUID to attach visualization to
    */
  def upsert(
    visualization: Visualization,
    specificId: ExpressionId = null
  ): Unit = {
    val id = if (specificId == null) {
      visualization.expressionId
    } else {
      specificId
    }
    val visualizations = visualizationMap(id)
    val rest           = visualizations.filterNot(_.id == visualization.id)
    visualizationMap.update(id, visualization :: rest)
  }

  /** Removes a visualization from the holder.
    *
    * @param visualizationId the visualization identifier
    * @param expressionId the id of expression that the visualization is
    *                     attached to
    */
  def remove(
    visualizationId: VisualizationId,
    expressionId: ExpressionId
  ): Unit = {
    val visualizations = visualizationMap(expressionId)
    val rest           = visualizations.filterNot(_.id == visualizationId)
    visualizationMap.update(expressionId, rest)
  }

  /** Finds all visualizations attached to an expression.
    *
    * @param expressionId the unique identifier of the expression
    * @return a list of matching visualization
    */
  def find(expressionId: ExpressionId): List[Visualization] =
    visualizationMap(expressionId)

  /** Finds all visualizations in a given module.
    *
    * @param module the qualified module name
    * @return a list of matching visualization
    */
  def findByModule(
    module: QualifiedName
  ): Iterable[Visualization] =
    visualizationMap.values.flatten.collect {
      case visualization: Visualization
          if visualization.module.getName == module =>
        visualization
    }

  /** Returns a visualization with the provided id.
    *
    * @param visualizationId the identifier of visualization
    * @return an option with visualization
    */
  def getById(visualizationId: VisualizationId): Option[Visualization] =
    visualizationMap.values.flatten.find(_.id == visualizationId)

  /** @return all available visualizations. */
  def getAll: Iterable[Visualization] =
    visualizationMap.values.flatten

  /** @return the oneshot expression attached to the `expressionId`. */
  def getOneshotExpression(
    expressionId: ExpressionId
  ): OneshotExpression = {
    if (
      oneshotExpression != null && oneshotExpression.expressionId == expressionId
    ) {
      return oneshotExpression
    }

    null
  }

  /** Set oneshot expression for execution. */
  def setOneshotExpression(oneshotExpression: OneshotExpression): Unit = {
    this.oneshotExpression = oneshotExpression
  }
}

object VisualizationHolder {

  /** Returns an empty visualization holder. */
  def empty = new VisualizationHolder

}
