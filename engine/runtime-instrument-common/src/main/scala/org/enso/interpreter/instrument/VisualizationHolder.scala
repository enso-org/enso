package org.enso.interpreter.instrument

import org.enso.pkg.QualifiedName
import org.enso.polyglot.runtime.Runtime.Api.{ExpressionId, VisualizationId}
import com.oracle.truffle.api.CompilerDirectives

import scala.collection.mutable

/** A mutable holder of all visualizations attached to an execution context.
  * This class is thread-safe.
  */
class VisualizationHolder {

  private val oneshotExpressions: mutable.Map[ExpressionId, OneshotExpression] =
    mutable.Map.empty

  private val visualizationMap: mutable.Map[ExpressionId, List[Visualization]] =
    mutable.Map.empty.withDefaultValue(List.empty)

  private val unevaluatedMap
    : mutable.Map[ExpressionId, List[UnevaluatedVisualization]] =
    mutable.Map.empty.withDefaultValue(List.empty)

  private val pendingSubExpressionVisualizations: mutable.Set[ExpressionId] =
    mutable.HashSet.empty

  /** Upserts a visualization.
    *
    * @param visualization the visualization to upsert
    * @param specificId UUID to attach visualization to
    */
  def upsert(
    visualization: Visualization,
    specificId: ExpressionId = null
  ): Unit = synchronized {
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
  ): Unit = synchronized {
    val visualizations = visualizationMap(expressionId)
    val rest           = visualizations.filterNot(_.id == visualizationId)
    visualizationMap.update(expressionId, rest)
  }

  /** Finds all visualizations attached to an expression.
    *
    * @param expressionId the unique identifier of the expression
    * @return a list of matching visualization
    */
  @CompilerDirectives.TruffleBoundary
  def find(expressionId: ExpressionId): List[Visualization] = synchronized {
    visualizationMap(expressionId)
  }

  /** Finds all visualizations in a given module.
    *
    * @param module the qualified module name
    * @return a list of matching visualization
    */
  def findByModule(
    module: QualifiedName
  ): Iterable[Visualization] = synchronized {
    visualizationMap.values.flatten.collect {
      case visualization: Visualization
          if visualization.module.getName == module =>
        visualization
    }.toList
  }

  /** Returns a visualization with the provided id.
    *
    * @param visualizationId the identifier of visualization
    * @return an option with visualization
    */
  def getById(visualizationId: VisualizationId): Option[Visualization] =
    synchronized {
      visualizationMap.values.flatten.find(_.id == visualizationId)
    }

  /** @return all available visualizations. */
  def getAll: Iterable[Visualization] = synchronized {
    visualizationMap.values.flatten.toList
  }

  /** @return the oneshot expression attached to the `expressionId`. */
  def getOneshotExpression(
    expressionId: ExpressionId
  ): OneshotExpression = synchronized {
    oneshotExpressions.remove(expressionId).orNull
  }

  /** Set oneshot expression for execution. */
  def setOneshotExpression(oneshotExpression: OneshotExpression): Unit =
    synchronized {
      this.oneshotExpressions
        .put(oneshotExpression.expressionId, oneshotExpression)
    }

  /** Returns an unevaluated visualization with the provided id.
    *
    * @param visualizationId the identifier of visualization
    * @return an option with unevaluated visualization
    */
  def getUnevaluatedById(
    visualizationId: VisualizationId
  ): Option[UnevaluatedVisualization] =
    synchronized {
      unevaluatedMap.values.flatten.find(_.id == visualizationId)
    }

  /** Upserts an unevaluated visualization. Multiple can exist per expression. */
  def upsertUnevaluated(unevaluated: UnevaluatedVisualization): Unit =
    synchronized {
      val existing = unevaluatedMap(unevaluated.expressionId)
      val rest     = existing.filterNot(_.id == unevaluated.id)
      unevaluatedMap.update(unevaluated.expressionId, unevaluated :: rest)
    }

  /** Removes an unevaluated visualization by ID from a specific expression.
    *
    * @param visualizationId the visualization identifier
    * @param expressionId the id of expression that the visualization is attached to
    * @return the removed unevaluated visualization, if found
    */
  def removeUnevaluated(
    visualizationId: VisualizationId,
    expressionId: ExpressionId
  ): Option[UnevaluatedVisualization] = synchronized {
    val existing        = unevaluatedMap(expressionId)
    val (removed, rest) = existing.partition(_.id == visualizationId)
    unevaluatedMap.update(expressionId, rest)
    removed.headOption
  }

  /** Gets all unevaluated visualizations for an expression.
    *
    * @param expressionId the unique identifier of the expression
    * @return a list of matching unevaluated visualizations
    */
  @CompilerDirectives.TruffleBoundary
  def findUnevaluated(
    expressionId: ExpressionId
  ): List[UnevaluatedVisualization] = synchronized {
    unevaluatedMap(expressionId)
  }

  /** Gets all unevaluated visualizations across all expressions. */
  @CompilerDirectives.TruffleBoundary
  def getAllUnevaluated: Iterable[UnevaluatedVisualization] = synchronized {
    unevaluatedMap.values.flatten.toList
  }

  /** Checks if there are any pending unevaluated visualizations.
    *
    * @return true if there are pending visualizations to process
    */
  def hasPendingVisualizations: Boolean = synchronized {
    unevaluatedMap.values.exists(_.nonEmpty)
  }

  /** Registers a nested visualization for a subexpression.
    *
    * @param nodeID the expression id to register
    */
  @CompilerDirectives.TruffleBoundary
  def upsertNestedVisualization(nodeID: ExpressionId): Unit = {
    pendingSubExpressionVisualizations.add(nodeID)
  }

  /** Checks if there is a pending nested visualization for the given expression.
    * If found, removes it from the pending set.
    *
    * @param nodeID the expression id to check
    * @return true if a nested visualization was pending for this expression
    */
  @CompilerDirectives.TruffleBoundary
  def checkAndClearNestedVisualizations(nodeID: ExpressionId): Boolean = {
    pendingSubExpressionVisualizations.remove(nodeID)
  }
}

object VisualizationHolder {

  /** Returns an empty visualization holder. */
  def empty = new VisualizationHolder
}
