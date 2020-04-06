package org.enso.languageserver.runtime

import akka.actor.ActorRef
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
    * Signals that user doesn't have access to the requested context.
    */
  case object AccessDenied extends Failure

  /**
    * Signals that context was not found.
    */
  case class ContextNotFound(contextId: ContextId) extends Failure
}
