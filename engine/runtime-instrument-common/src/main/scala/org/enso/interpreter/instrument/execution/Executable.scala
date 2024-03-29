package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.InstrumentFrame
import org.enso.polyglot.runtime.Runtime.Api

import scala.collection.mutable

/** Represents executable piece of Enso program.
  *
  * @param contextId an identifier of a context to execute
  * @param stack a call stack that must be executed
  */
case class Executable(
  contextId: Api.ContextId,
  stack: mutable.Stack[InstrumentFrame]
)
