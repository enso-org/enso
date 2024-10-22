package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.ExpressionExecutionState

/** The state of the runtime */
final class ExecutionState {

  /** The storage for pending file edits */
  val pendingEdits: PendingEdits = new PendingFileEdits()

  val executionHooks: ExecutionHooks = new RuntimeExecutionHooks

  val expressionExecutionState = new ExpressionExecutionState()

  val suggestions: ModuleIndexing = ModuleIndexing.createInstance()
}
