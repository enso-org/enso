package org.enso.languageserver.runtime

import akka.actor.ActorRef
import org.enso.languageserver.filemanager.FileSystemFailure
import org.enso.languageserver.runtime.ExecutionApi.ContextId

object ContextRegistryProtocol {

  /**
    * Trait indicating failure response.
    */
  sealed trait Failure

  /**
    * A request to the context registry to create a new execution context.
    *
    * @param client reference to the client
    */
  case class CreateContextRequest(client: ActorRef)

  /**
    * A response about creation of a new execution context.
    *
    * @param contextId the newly created context's id
    */
  case class CreateContextResponse(contextId: ContextId)

  /**
    * A request to the context registry to delete an execution context.
    *
    * @param client reference to the client
    */
  case class DestroyContextRequest(client: ActorRef, contextId: ContextId)

  /**
    * A response about deletion of an execution context.
    *
    * @param contextId the newly created context's id
    */
  case class DestroyContextResponse(contextId: ContextId)

  /**
    * A request to the context registry to push an execution context
    * down the stack.
    *
    * @param client reference to the client
    * @param contextId execution context identifier
    * @param stackItem an object representing an item on the stack
    */
  case class PushContextRequest(
    client: ActorRef,
    contextId: ContextId,
    stackItem: StackItem
  )

  /**
    * A response about pushing the new item to the stack.
    *
    * @param contextId execution context identifier
    */
  case class PushContextResponse(contextId: ContextId)

  /**
    * A request to the context registry to move an execution context
    * up the stack.
    *
    * @param client reference to the client
    * @param contextId execution context identifier
    */
  case class PopContextRequest(client: ActorRef, contextId: ContextId)

  /**
    * A response about popping the stack.
    *
    * @param contextId execution context identifier
    */
  case class PopContextResponse(contextId: ContextId)

  /**
    * Signals that user doesn't have access to the requested context.
    */
  case object AccessDenied extends Failure

  /**
    * Signals that context was not found.
    *
    * @param contextId execution context identifier
    */
  case class ContextNotFound(contextId: ContextId) extends Failure

  /**
    * Signals about file system error.
    *
    * @param error file system failure
    */
  case class FileSystemError(error: FileSystemFailure) extends Failure

  /**
    * Signals that stack is empty.
    *
    * @param contextId execution context identifier
    */
  case class EmptyStackError(contextId: ContextId) extends Failure
}
