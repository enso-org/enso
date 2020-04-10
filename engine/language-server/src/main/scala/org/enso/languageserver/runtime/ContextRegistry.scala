package org.enso.languageserver.runtime

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.data.Config
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
  *   - *H - Request is forwarded to intermediate handler. Created per request.
  *
  * {{{
  *
  *                   *C                            1
  *  +------------------+   *H    +------------------+
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
    withStore(ContextRegistry.Store(Map()))

  private def withStore(store: ContextRegistry.Store): Receive = {
    case Ping =>
      sender() ! Pong

    case CreateContextRequest(client) =>
      val handler =
        context.actorOf(CreateContextHandler.props(timeout, runtime))
      val contextId = UUID.randomUUID()
      handler.forward(Api.CreateContextRequest(contextId))
      context.become(withStore(store.addContext(client, contextId)))

    case DestroyContextRequest(client, contextId) =>
      val contexts = store.getContexts(client)
      if (contexts.contains(contextId)) {
        val handler =
          context.actorOf(DestroyContextHandler.props(timeout, runtime))
        handler.forward(Api.DestroyContextRequest(contextId))
        context.become(withStore(store.updated(client, contexts - contextId)))
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
        file          = pointer.file.toFile(rootPath).toPath,
        definedOnType = pointer.definedOnType,
        name          = pointer.name
      )
    }

}

object ContextRegistry {

  private type ClientRef = ActorRef

  private case class Store(store: Map[ClientRef, Set[ContextId]]) {

    def addContext(client: ClientRef, contextId: ContextId): Store =
      copy(store = store + (client -> (getContexts(client) + contextId)))

    def updated(client: ClientRef, contexts: Set[ContextId]): Store =
      copy(store = store.updated(client, contexts))

    def getContexts(client: ClientRef): Set[ContextId] =
      store.getOrElse(client, Set())

    def hasContext(client: ClientRef, contextId: ContextId): Boolean =
      getContexts(client).contains(contextId)
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
