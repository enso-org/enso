package org.enso.interpreter.instrument.execution

/** The state of the runtime.
  *
  * @param pendingEdits the storage for pending file edits
  */
final class ExecutionState(
  val pendingEdits: PendingEdits = new PendingFileEdits()
)
