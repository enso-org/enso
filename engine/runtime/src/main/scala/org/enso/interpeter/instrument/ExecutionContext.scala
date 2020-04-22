package org.enso.interpeter.instrument

import org.enso.polyglot.runtime.Runtime.Api.{ContextId, StackItem}

import scala.collection.mutable.Stack

/**
  * Storage for active execution contexts.
  */
class ExecutionContextManager {
  private var contexts: Map[ContextId, Stack[StackItem]] = Map()

  /**
    * Creates a new context with a given id.
    *
    * @param id the context id.
    */
  def create(id: ContextId): Unit =
    contexts += id -> Stack.empty

  /**
    * Destroys a context with a given id.
    * @param id the context id.
    */
  def destroy(id: ContextId): Unit =
    contexts -= id

  /**
    * Gets a context with a given id.
    *
    * @param id the context id.
    * @return the context with the given id, if exists.
    */
  def get(id: ContextId): Option[ContextId] =
    for {
      _ <- contexts.get(id)
    } yield id

  /**
    * Gets a stack for a given context id.
    *
    * @param id the context id.
    * @return the stack.
    */
  def getStack(id: ContextId): Stack[StackItem] =
    contexts.getOrElse(id, Stack())

  /**
    * Gets all execution contexts.
    *
    * @return all currently available execution contexsts.
    */
  def getAll: collection.MapView[ContextId, Stack[StackItem]] =
    contexts.view

  /**
    * If the context exists, push the item on the stack.
    *
    * @param id the context id.
    * @param item stack item.
    * @return Unit representing success or None if the context does not exist.
    */
  def push(id: ContextId, item: StackItem): Option[Unit] =
    for {
      stack <- contexts.get(id)
    } yield stack.push(item)

  /**
    * If the context exists and stack not empty, pop the item from the stack.
    *
    * @param id the context id.
    * @return stack item or None if the stack is empty or not exists.
    */
  def pop(id: ContextId): Option[StackItem] =
    for {
      stack <- contexts.get(id)
      if stack.nonEmpty
    } yield stack.pop()
}
