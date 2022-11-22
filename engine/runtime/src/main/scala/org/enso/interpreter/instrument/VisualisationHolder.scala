package org.enso.interpreter.instrument

import org.enso.pkg.QualifiedName
import org.enso.polyglot.runtime.Runtime.Api.{ExpressionId, VisualisationId}

import scala.collection.mutable

/** A mutable holder of all visualisations attached to an execution context.
  */
class VisualisationHolder() {

  private val visualisationMap: mutable.Map[ExpressionId, List[Visualisation]] =
    mutable.Map.empty.withDefaultValue(List.empty)

  /** Upserts a visualisation.
    *
    * @param visualisation the visualisation to upsert
    */
  def upsert(visualisation: Visualisation): Unit = {
    val visualisations = visualisationMap(visualisation.expressionId)
    val rest           = visualisations.filterNot(_.id == visualisation.id)
    visualisationMap.update(visualisation.expressionId, visualisation :: rest)
  }

  /** Removes a visualisation from the holder.
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
    val rest           = visualisations.filterNot(_.id == visualisationId)
    visualisationMap.update(expressionId, rest)
  }

  /** Finds all visualisations attached to an expression.
    *
    * @param expressionId the unique identifier of the expression
    * @return a list of matching visualisation
    */
  def find(expressionId: ExpressionId): List[Visualisation] =
    visualisationMap(expressionId)

  /** Finds all visualisations in a given module.
    *
    * @param module the qualified module name
    * @return a list of matching visualisation
    */
  def findByModule(module: QualifiedName): Iterable[Visualisation] =
    visualisationMap.values.flatten.filter(_.module.getName == module)

  /** Returns a visualisation with the provided id.
    *
    * @param visualisationId the identifier of visualisation
    * @return an option with visualisation
    */
  def getById(visualisationId: VisualisationId): Option[Visualisation] =
    visualisationMap.values.flatten.find(_.id == visualisationId)

  /** @return all available visualisations. */
  def getAll: Iterable[Visualisation] =
    visualisationMap.values.flatten
}

object VisualisationHolder {

  /** Returns an empty visualisation holder. */
  def empty = new VisualisationHolder()

}
