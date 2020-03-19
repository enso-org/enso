package org.enso.interpeter.instrument

import org.enso.polyglot.runtime.Runtime.Api.ContextId

/**
  * Represents an execution context.
  *
  * @param id the context id.
  */
case class ExecutionContext(id: ContextId)

/**
  * Storage for active execution contexts.
  */
class ExecutionContextManager {
  private var contexts: List[ExecutionContext] = List()

  /**
    * Creates a new context with a given id.
    *
    * @param id the context id.
    */
  def create(id: ContextId): Unit = contexts ::= ExecutionContext(id)

  /**
    * Destroys a context with a given id.
    * @param id the context id.
    */
  def destroy(id: ContextId): Unit = contexts = contexts.filter(_.id != id)

  /**
    * Gets a context with a given id.
    *
    * @param id the context id.
    * @return the context with the given id, if exists.
    */
  def get(id: ContextId): Option[ExecutionContext] = contexts.find(_.id == id)
}
