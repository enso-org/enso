package org.enso.languageserver.runtime

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.languageserver.data.ExecutionContextConfig
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
  * @param config execution context configuration
  * @param runtime reference to the [[RuntimeConnector]]
  */
final class ContextRegistry(config: ExecutionContextConfig, runtime: ActorRef)
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  import ContextRegistry._, ContextRegistryProtocol._

  override def receive: Receive =
    withStore(Store(Map()))

  private def withStore(store: Store): Receive = {
    case CreateContextRequest(client) =>
      val handler = context.actorOf(
        CreateContextHandler.props(config.requestTimeout, runtime)
      )
      val contextId = UUID.randomUUID()
      handler.forward(Api.CreateContextRequest(contextId))
      context.become(withStore(store.addContext(client, contextId)))

    case DestroyContextRequest(client, contextId) =>
      val contexts = store.getContexts(client)
      if (contexts.contains(contextId)) {
        val handler = context.actorOf(
          DestroyContextHandler.props(config.requestTimeout, runtime)
        )
        handler.forward(Api.DestroyContextRequest(contextId))
        context.become(withStore(store.updated(client, contexts - contextId)))
      } else {
        sender() ! AccessDenied
      }
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
  }

  /**
    * Creates a configuration object used to create a [[ContextRegistry]].
    *
    * @param config execution context configuration
    * @param runtime reference to the [[RuntimeConnector]]
    */
  def props(config: ExecutionContextConfig, runtime: ActorRef): Props =
    Props(new ContextRegistry(config, runtime))
}
