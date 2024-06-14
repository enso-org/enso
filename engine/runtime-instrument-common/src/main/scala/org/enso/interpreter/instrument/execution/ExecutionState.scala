package org.enso.interpreter.instrument.execution

/** The state of the runtime */
final class ExecutionState {

  /** The storage for pending file edits */
  val pendingEdits: PendingEdits = new PendingFileEdits()

  val suggestions: ModuleIndexing = ModuleIndexing.createInstance()
}
