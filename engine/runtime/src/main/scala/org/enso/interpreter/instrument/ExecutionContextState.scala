package org.enso.interpreter.instrument

import org.enso.polyglot.runtime.Runtime.Api.StackItem

import scala.collection.mutable

/** Represents a state of an execution context.
  *
  * @param stack the current call stack for the execution context
  * @param visualisations the holder of all visualisations attached to the
  *                       execution context
  */
case class ExecutionContextState(
  stack: mutable.Stack[InstrumentFrame],
  visualisations: VisualisationHolder
)

object ExecutionContextState {

  /** Returns empty state.
    */
  def empty: ExecutionContextState =
    ExecutionContextState(mutable.Stack.empty, VisualisationHolder.empty)
}

/** Stack frame of the context.
  *
  * @param item the stack item.
  * @param cache the cache of this stack frame.
  * @param syncState the synchronization state of runtime updates.
  */
case class InstrumentFrame(
  item: StackItem,
  cache: RuntimeCache,
  syncState: UpdatesSynchronizationState
)

case object InstrumentFrame {

  def apply(item: StackItem): InstrumentFrame =
    new InstrumentFrame(item, new RuntimeCache, new UpdatesSynchronizationState)
}
