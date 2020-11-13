package org.enso.interpreter.instrument.execution

import com.oracle.truffle.api.TruffleContext
import org.enso.interpreter.instrument.{Endpoint, ExecutionContextManager}
import org.enso.interpreter.service.ExecutionService
import org.enso.text.ContentBasedVersioning

/** Contains suppliers of services that provide application specific
  * functionality.
  *
  * @param executionService a service allowing externally-triggered code
  * execution
  * @param contextManager a storage for active execution contexts
  * @param endpoint a message endpoint
  * @param truffleContext a context of a set of Truffle languages
  * @param jobProcessor a processor responsible for executing jobs
  * @param jobControlPlane a job control plane
  * @param locking a locking service
  * @param versioning a version calculator
  */
case class RuntimeContext(
  executionService: ExecutionService,
  contextManager: ExecutionContextManager,
  endpoint: Endpoint,
  truffleContext: TruffleContext,
  jobProcessor: JobProcessor,
  jobControlPlane: JobControlPlane,
  locking: Locking,
  versioning: ContentBasedVersioning
)
