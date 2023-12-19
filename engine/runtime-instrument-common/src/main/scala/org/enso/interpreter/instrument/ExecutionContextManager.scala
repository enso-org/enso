package org.enso.interpreter.instrument

import org.enso.pkg.QualifiedName
import org.enso.polyglot.runtime.Runtime.Api.{
  ContextId,
  ExpressionId,
  StackItem,
  VisualizationId
}

import scala.collection.mutable

/** Storage for active execution contexts.
  */
class ExecutionContextManager {

  private var contexts: Map[ContextId, ExecutionContextState] =
    Map().withDefaultValue(ExecutionContextState.empty)

  /** Creates a new context with a given id.
    *
    * @param id the context id.
    */
  def create(id: ContextId): Unit =
    synchronized {
      contexts += id -> ExecutionContextState.empty
    }

  /** Destroys a context with a given id.
    * @param id the context id.
    */
  def destroy(id: ContextId): Unit =
    synchronized {
      contexts -= id
    }

  /** Gets a stack for a given context id.
    *
    * @param id the context id.
    * @return the stack.
    */
  def getStack(id: ContextId): mutable.Stack[InstrumentFrame] =
    synchronized {
      contexts(id).stack
    }

  /** Gets all execution contexts.
    *
    * @return all currently available execution contexts.
    */
  def getAllContexts
    : collection.MapView[ContextId, mutable.Stack[InstrumentFrame]] =
    synchronized {
      contexts.view.mapValues(_.stack)
    }

  /** If the context exists, push the item on the stack.
    *
    * @param id the context id.
    * @param item stack item.
    * @return Unit representing success or None if the context does not exist.
    */
  def push(id: ContextId, item: StackItem): Option[Unit] =
    synchronized {
      for {
        state <- contexts.get(id)
      } yield state.stack.push(InstrumentFrame(item))
    }

  /** If the context exists and stack not empty, pop the item from the stack.
    *
    * @param id the context id.
    * @return stack frame or None if the stack is empty or not exists.
    */
  def pop(id: ContextId): Option[InstrumentFrame] =
    synchronized {
      for {
        state <- contexts.get(id)
        if state.stack.nonEmpty
      } yield state.stack.pop()
    }

  /** Tests if a context specified by its id is stored by the manager.
    *
    * @param contextId the identifier of the execution context
    * @return true if the context is stored or false otherwise
    */
  def contains(contextId: ContextId): Boolean =
    synchronized {
      contexts.contains(contextId)
    }

  /** Upserts a visualization for the specified context.
    *
    * @param contextId the identifier of the execution context
    * @param visualization the visualization to upsert
    */
  def upsertVisualization(
    contextId: ContextId,
    visualization: Visualization
  ): Unit =
    synchronized {
      val state = contexts(contextId)
      state.visualizations.upsert(visualization)
    }

  /** Gets a context with a given id.
    *
    * @param id the context id.
    * @return the context with the given id, if exists.
    */
  def getVisualizationHolder(id: ContextId): VisualizationHolder =
    synchronized {
      contexts.get(id).map(_.visualizations).getOrElse(new VisualizationHolder)
    }

  /** Get visualizations of all execution contexts. */
  def getAllVisualizations: Iterable[Visualization] =
    synchronized {
      contexts.values.flatMap(_.visualizations.getAll)
    }

  /** Get visualizations defined in the module.
    *
    * @param module the qualified module name
    * @return the list of matching visualizations
    */
  def getVisualizations(module: QualifiedName): Iterable[Visualization] =
    synchronized {
      contexts.values.flatMap(_.visualizations.findByModule(module))
    }

  /** Returns a visualization with the provided id.
    *
    * @param contextId the identifier of the execution context
    * @param visualizationId the identifier of visualization
    * @return an option with visualization
    */
  def getVisualizationById(
    contextId: ContextId,
    visualizationId: VisualizationId
  ): Option[Visualization] =
    synchronized {
      for {
        state         <- contexts.get(contextId)
        visualization <- state.visualizations.getById(visualizationId)
      } yield visualization
    }

  /** Finds all visualizations attached to an expression.
    *
    * @param contextId the identifier of the execution context
    * @param expressionId the unique identifier of the expression
    * @return a list of matching visualization
    */
  def findVisualizationForExpression(
    contextId: ContextId,
    expressionId: ExpressionId
  ): List[Visualization] =
    synchronized {
      for {
        state         <- contexts.get(contextId).toList
        visualization <- state.visualizations.find(expressionId)
      } yield visualization
    }

  /** Get all visualizations invalidated by the provided list of expressions.
    *
    * @param module the module containing the visualizations
    * @param invalidatedExpressions the list of invalidated expressions
    * @return a list of matching visualization
    */
  def getInvalidatedVisualizations(
    module: QualifiedName,
    invalidatedExpressions: Set[ExpressionId]
  ): Iterable[Visualization] = {
    for {
      state         <- contexts.values
      visualization <- state.visualizations.findByModule(module)
      if visualization.visualizationExpressionId.exists(
        invalidatedExpressions.contains
      )
    } yield visualization
  }

  /** Removes a visualization from the holder.
    *
    * @param contextId the identifier of the execution context
    * @param visualizationId the visualization identifier
    * @param expressionId the id of expression that the visualization is
    *                     attached to
    */
  def removeVisualization(
    contextId: ContextId,
    expressionId: ExpressionId,
    visualizationId: VisualizationId
  ): Unit =
    synchronized {
      val state = contexts(contextId)
      state.visualizations.remove(visualizationId, expressionId)
    }

}
