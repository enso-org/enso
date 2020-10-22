package org.enso.interpreter.instrument

import org.enso.polyglot.runtime.Runtime.Api.{
  ContextId,
  ExpressionId,
  StackItem,
  VisualisationId
}

import scala.collection.mutable.Stack

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

  /** Gets a context with a given id.
    *
    * @param id the context id.
    * @return the context with the given id, if exists.
    */
  def get(id: ContextId): Option[ContextId] =
    synchronized {
      for {
        _ <- contexts.get(id)
      } yield id
    }

  /** Gets a stack for a given context id.
    *
    * @param id the context id.
    * @return the stack.
    */
  def getStack(id: ContextId): Stack[InstrumentFrame] =
    synchronized {
      contexts(id).stack
    }

  /** Gets all execution contexts.
    *
    * @return all currently available execution contexsts.
    */
  def getAll: collection.MapView[ContextId, Stack[InstrumentFrame]] =
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

  /** Upserts a visualisation for the specified context.
    *
    * @param contextId the identifier of the execution context
    * @param visualisation the visualisation to upsert
    */
  def upsertVisualisation(
    contextId: ContextId,
    visualisation: Visualisation
  ): Unit =
    synchronized {
      val state = contexts(contextId)
      state.visualisations.upsert(visualisation)
    }

  /** Returns a visualisation with the provided id.
    *
    * @param contextId the identifier of the execution context
    * @param visualisationId the identifier of visualisation
    * @return an option with visualisation
    */
  def getVisualisationById(
    contextId: ContextId,
    visualisationId: VisualisationId
  ): Option[Visualisation] =
    synchronized {
      for {
        state         <- contexts.get(contextId)
        visualisation <- state.visualisations.getById(visualisationId)
      } yield visualisation
    }

  /** Finds all visualisations attached to an expression.
    *
    * @param contextId the identifier of the execution context
    * @param expressionId the unique identifier of the expression
    * @return a list of matching visualisation
    */
  def findVisualisationForExpression(
    contextId: ContextId,
    expressionId: ExpressionId
  ): List[Visualisation] =
    synchronized {
      for {
        state         <- contexts.get(contextId).toList
        visualisation <- state.visualisations.find(expressionId)
      } yield visualisation
    }

  /** Removes a visualisation from the holder.
    *
    * @param contextId the identifier of the execution context
    * @param visualisationId the visualisation identifier
    * @param expressionId the id of expression that the visualisation is
    *                     attached to
    */
  def removeVisualisation(
    contextId: ContextId,
    expressionId: ExpressionId,
    visualisationId: VisualisationId
  ): Unit =
    synchronized {
      val state = contexts(contextId)
      state.visualisations.remove(visualisationId, expressionId)
    }

}
