package org.enso.languageserver.runtime

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.data.{Client, Config}
import org.enso.languageserver.filemanager.FileSystemFailure
import org.enso.languageserver.runtime.ExecutionApi.ContextId
import org.enso.languageserver.runtime.handler._
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

/**
  * Registry handles execution context requests and communicates with runtime
  * connector.
  *
  * == Implementation ==
  *
  * Legend:
  *
  *   - 1  - Singleton
  *   - *C - Created per client.
  *   - *X - Created per context.
  *   - *H - Request is forwarded to intermediate handler. Created per request.
  *
  * {{{
  *
  *                                                     *X
  *                               +-----------------------+
  *            +------------------+ ContextEventsListener |
  *            |                  +-----------------------+
  *            |
  *            |
  *            v      *C                            1
  *  +---------+--------+   *H    +------------------+
  *  | ClientController +----+--->+ ContextRegistry  |
  *  +------------------+    ^    +---------+--------+
  *                          |              |
  *                          +--------------+*H
  *                                         v       1
  *                               +---------+--------+
  *                               | RuntimeConnector |
  *                               +------------------+
  *
  * }}}
  *
  * @param config configuration
  * @param runtime reference to the [[RuntimeConnector]]
  */
final class ContextRegistry(config: Config, runtime: ActorRef)
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  import ContextRegistryProtocol._

  private val timeout = config.executionContext.requestTimeout

  override def receive: Receive =
    withStore(ContextRegistry.Store())

  private def withStore(store: ContextRegistry.Store): Receive = {
    case Ping =>
      sender() ! Pong

    case CreateContextRequest(client) =>
      val contextId = UUID.randomUUID()
      val handler =
        context.actorOf(CreateContextHandler.props(timeout, runtime))
      val listener =
        context.actorOf(ContextEventsListener.props(config, client, contextId))
      handler.forward(Api.CreateContextRequest(contextId))
      context.become(withStore(store.addContext(client, contextId, listener)))

    case DestroyContextRequest(client, contextId) =>
      if (store.hasContext(client, contextId)) {
        val handler =
          context.actorOf(DestroyContextHandler.props(timeout, runtime))
        store.getListener(contextId).foreach(context.stop)
        handler.forward(Api.DestroyContextRequest(contextId))
        context.become(withStore(store.removeContext(client, contextId)))
      } else {
        sender() ! AccessDenied
      }

    case PushContextRequest(client, contextId, stackItem) =>
      if (store.hasContext(client, contextId)) {
        getRuntimeStackItem(stackItem) match {
          case Right(stackItem) =>
            val handler =
              context.actorOf(PushContextHandler.props(timeout, runtime))
            handler.forward(Api.PushContextRequest(contextId, stackItem))
          case Left(error) =>
            sender() ! FileSystemError(error)
        }
      } else {
        sender() ! AccessDenied
      }

    case PopContextRequest(client, contextId) =>
      if (store.hasContext(client, contextId)) {
        val handler = context.actorOf(PopContextHandler.props(timeout, runtime))
        handler.forward(Api.PopContextRequest(contextId))
      } else {
        sender() ! AccessDenied
      }
  }

  private def getRuntimeStackItem(
    stackItem: StackItem
  ): Either[FileSystemFailure, Api.StackItem] =
    stackItem match {
      case StackItem.ExplicitCall(pointer, argument, arguments) =>
        getRuntimeMethodPointer(pointer).map { methodPointer =>
          Api.StackItem.ExplicitCall(methodPointer, argument, arguments)
        }

      case StackItem.LocalCall(expressionId) =>
        Right(Api.StackItem.LocalCall(expressionId))
    }

  private def getRuntimeMethodPointer(
    pointer: MethodPointer
  ): Either[FileSystemFailure, Api.MethodPointer] =
    config.findContentRoot(pointer.file.rootId).map { rootPath =>
      Api.MethodPointer(
        file          = pointer.file.toFile(rootPath),
        definedOnType = pointer.definedOnType,
        name          = pointer.name
      )
    }

}

object ContextRegistry {

  private case class Store(
    contexts: Map[Client, Set[ContextId]],
    listeners: Map[ContextId, ActorRef]
  ) {

    def addContext(
      client: Client,
      contextId: ContextId,
      listener: ActorRef
    ): Store =
      copy(
        contexts  = contexts + (client     -> (getContexts(client) + contextId)),
        listeners = listeners + (contextId -> listener)
      )

    def removeContext(client: Client, contextId: ContextId): Store =
      copy(
        contexts  = contexts.updated(client, getContexts(client) - contextId),
        listeners = listeners - contextId
      )

    def getContexts(client: Client): Set[ContextId] =
      contexts.getOrElse(client, Set())

    def getListener(contextId: ContextId): Option[ActorRef] =
      listeners.get(contextId)

    def hasContext(client: Client, contextId: ContextId): Boolean =
      getContexts(client).contains(contextId)
  }

  private object Store {

    def apply(): Store =
      Store(Map(), Map())
  }

  /**
    * Creates a configuration object used to create a [[ContextRegistry]].
    *
    * @param config language server configuration
    * @param runtime reference to the [[RuntimeConnector]]
    */
  def props(config: Config, runtime: ActorRef): Props =
    Props(new ContextRegistry(config, runtime))
}
