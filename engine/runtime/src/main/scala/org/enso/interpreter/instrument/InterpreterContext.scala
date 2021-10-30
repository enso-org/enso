package org.enso.interpreter.instrument

import com.oracle.truffle.api.TruffleContext
import org.enso.interpreter.service.ExecutionService

/** Contains suppliers of services that provide interpreter specific
  * functionality.
  *
  * @param executionService a service allowing externally-triggered code
  *                         execution
  * @param contextManager a storage for active execution contexts
  * @param endpoint a message endpoint
  * @param truffleContext a context of a set of Truffle languages
  */
case class InterpreterContext(
  executionService: ExecutionService,
  contextManager: ExecutionContextManager,
  endpoint: Endpoint,
  truffleContext: TruffleContext
)
