package org.enso.interpeter.instrument

import org.enso.polyglot.runtime.Runtime.Api.StackItem

import scala.collection.mutable.Stack

/**
  * Represents a state of an execution context.
  *
  * @param stack the current call stack for the execution context
  * @param visualisations the holder of all visualisations attached to the
  *                       execution context
  */
case class ExecutionContextState(
  stack: Stack[StackItem],
  visualisations: VisualisationHolder
)

object ExecutionContextState {

  /**
    * Returns empty state.
    */
  def empty = ExecutionContextState(Stack.empty, VisualisationHolder.empty)

}
