package org.enso.languageserver.event

import java.util.UUID

import org.enso.languageserver.data.ClientId

/** Base trait for all execution context events.
  */
sealed trait ExecutionContextEvent extends Event

/** Notifies the Language Server about a new execution context.
  *
  * @param contextId the id of newly created context
  * @param owner the id of the owner of the context
  */
case class ExecutionContextCreated(contextId: UUID, owner: ClientId)
    extends ExecutionContextEvent

/** Notifies the Language Server about a termination of an execution context.
  *
  * @param contextId the id of terminated context
  * @param owner the id of the owner of the context
  */
case class ExecutionContextDestroyed(contextId: UUID, owner: ClientId)
    extends ExecutionContextEvent
