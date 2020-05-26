package org.enso.interpreter.instrument.execution

import com.oracle.truffle.api.TruffleContext
import org.enso.interpreter.instrument.{
  Cache,
  Endpoint,
  ExecutionContextManager
}
import org.enso.interpreter.service.ExecutionService

/**
  * Contains suppliers of services that provide application specific
  * functionality.
  *
  * @param executionService a service allowing externally-triggered code
  *                         execution
  * @param contextManager a storage for active execution contexts
  * @param endpoint a message endpoint
  * @param truffleContext a context of a set of Truffle languages
  * @param cache a storage for computed values
  * @param commandProcessor a component responsible for executing commands
  */
case class RuntimeContext(
  executionService: ExecutionService,
  contextManager: ExecutionContextManager,
  endpoint: Endpoint,
  truffleContext: TruffleContext,
  cache: Cache,
  commandProcessor: CommandProcessor
)
