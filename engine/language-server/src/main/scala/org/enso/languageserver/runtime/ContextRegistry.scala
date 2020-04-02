package org.enso.languageserver.runtime

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.languageserver.data.ExecutionContextConfig
import org.enso.languageserver.runtime.ExecutionApi.ContextId
import org.enso.languageserver.runtime.handler._

/**
  * Registry handles execution context requests and routes them to the
  * appropriate context manager.
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
    with ActorLogging {

  import ContextRegistry._

  override def receive: Receive =
    withStore(Store(Map()))

  private def withStore(store: Store): Receive = {
    case ContextRegistryProtocol.CreateContextRequest(client) =>
      val handler = context.actorOf(
        CreateContextHandler.props(config.requestTimeout, runtime)
      )
      val contextId = UUID.randomUUID()
      handler.forward(ExecutionProtocol.CreateContextRequest(contextId))
      context.become(withStore(store.addContext(client, contextId)))
  }

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)
}

object ContextRegistry {

  private type ClientRef = ActorRef

  private case class Store(store: Map[ClientRef, Set[ContextId]]) {

    def addContext(client: ClientRef, contextId: ContextId): Store =
      copy(store = store + (client -> (getContexts(client) + contextId)))

    def removeContext(client: ClientRef, contextId: ContextId): Store =
      copy(store = store + (client -> (getContexts(client) - contextId)))

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
