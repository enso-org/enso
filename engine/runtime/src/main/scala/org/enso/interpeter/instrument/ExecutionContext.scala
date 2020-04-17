package org.enso.interpeter.instrument

import org.enso.polyglot.runtime.Runtime.Api.{ContextId, StackItem}

import scala.collection.mutable.Stack

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
  private var contexts: Map[ExecutionContext, Stack[StackItem]] = Map()

  /**
    * Creates a new context with a given id.
    *
    * @param id the context id.
    */
  def create(id: ContextId): Unit =
    contexts += ExecutionContext(id) -> Stack.empty

  /**
    * Destroys a context with a given id.
    * @param id the context id.
    */
  def destroy(id: ContextId): Unit =
    contexts -= ExecutionContext(id)

  /**
    * Gets a context with a given id.
    *
    * @param id the context id.
    * @return the context with the given id, if exists.
    */
  def get(id: ContextId): Option[ExecutionContext] =
    for {
      _ <- contexts.get(ExecutionContext(id))
    } yield ExecutionContext(id)

  /**
    * Gets a stack for a given context id.
    *
    * @param id the context id.
    * @return the stack.
    */
  def getStack(id: ContextId): Stack[StackItem] =
    contexts.getOrElse(ExecutionContext(id), Stack())

  /**
    * If the context exists, push the item on the stack.
    *
    * @param id the context id.
    * @param item stack item.
    * @return Unit representing success or None if the context does not exist.
    */
  def push(id: ContextId, item: StackItem): Option[Unit] =
    for {
      stack <- contexts.get(ExecutionContext(id))
    } yield stack.push(item)

  /**
    * If the context exists and stack not empty, pop the item from the stack.
    *
    * @param id the context id.
    * @return stack item or None if the stack is empty or not exists.
    */
  def pop(id: ContextId): Option[StackItem] =
    for {
      stack <- contexts.get(ExecutionContext(id))
      if stack.nonEmpty
    } yield stack.pop()
}
