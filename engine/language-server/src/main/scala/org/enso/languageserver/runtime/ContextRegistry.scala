package org.enso.languageserver.runtime

import java.util.UUID

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.data.{ClientId, Config}
import org.enso.languageserver.event.{
  ExecutionContextCreated,
  ExecutionContextDestroyed
}
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.runtime.handler._
import org.enso.languageserver.util.UnhandledLogging
import org.enso.logger.akka.ActorMessageLogging
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.ContextId
import org.enso.searcher.SuggestionsRepo

import scala.concurrent.Future

/** Registry handles execution context requests and communicates with runtime
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
  * @param repo the suggestions repo
  * @param config configuration
  * @param runtime reference to the [[RuntimeConnector]]
  * @param sessionRouter the session router
  */
final class ContextRegistry(
  repo: SuggestionsRepo[Future],
  config: Config,
  runtime: ActorRef,
  sessionRouter: ActorRef
) extends Actor
    with LazyLogging
    with ActorMessageLogging
    with UnhandledLogging {

  import ContextRegistryProtocol._

  private val timeout = config.executionContext.requestTimeout

  override def preStart(): Unit = {
    context.system.eventStream
      .subscribe(self, classOf[Api.ExpressionUpdates])
    context.system.eventStream
      .subscribe(self, classOf[Api.VisualisationUpdate])
    context.system.eventStream
      .subscribe(self, classOf[Api.ExecutionFailed])
    context.system.eventStream
      .subscribe(self, classOf[Api.ExecutionUpdate])
    context.system.eventStream
      .subscribe(self, classOf[Api.VisualisationEvaluationFailed])
  }

  override def receive: Receive =
    withStore(ContextRegistry.Store())

  private def pingHandler: Receive = { case Ping =>
    sender() ! Pong
  }

  private def withStore(store: ContextRegistry.Store): Receive =
    pingHandler orElse LoggingReceive {
      case update: Api.ExpressionUpdates =>
        store.getListener(update.contextId).foreach(_ ! update)

      case update: Api.VisualisationUpdate =>
        store
          .getListener(update.visualisationContext.contextId)
          .foreach(_ ! update)

      case update: Api.ExecutionFailed =>
        store.getListener(update.contextId).foreach(_ ! update)

      case update: Api.ExecutionUpdate =>
        store.getListener(update.contextId).foreach(_ ! update)

      case update: Api.VisualisationEvaluationFailed =>
        store.getListener(update.contextId).foreach(_ ! update)

      case CreateContextRequest(client) =>
        val contextId = UUID.randomUUID()
        val handler =
          context.actorOf(CreateContextHandler.props(config, timeout, runtime))
        val listener =
          context.actorOf(
            ContextEventsListener.props(
              config,
              repo,
              client,
              contextId,
              sessionRouter
            )
          )
        handler.forward(Api.CreateContextRequest(contextId))
        context.become(
          withStore(store.addContext(client.clientId, contextId, listener))
        )
        context.system.eventStream
          .publish(ExecutionContextCreated(contextId, client.clientId))

      case DestroyContextRequest(client, contextId) =>
        if (store.hasContext(client.clientId, contextId)) {
          val handler =
            context.actorOf(
              DestroyContextHandler.props(config, timeout, runtime)
            )
          store.getListener(contextId).foreach(context.stop)
          handler.forward(Api.DestroyContextRequest(contextId))
          context.become(
            withStore(store.removeContext(client.clientId, contextId))
          )
          context.system.eventStream
            .publish(ExecutionContextDestroyed(contextId, client.clientId))
        } else {
          sender() ! AccessDenied
        }

      case PushContextRequest(client, contextId, stackItem) =>
        if (store.hasContext(client.clientId, contextId)) {
          val item = getRuntimeStackItem(stackItem)
          val handler =
            context.actorOf(PushContextHandler.props(config, timeout, runtime))
          handler.forward(Api.PushContextRequest(contextId, item))

        } else {
          sender() ! AccessDenied
        }

      case PopContextRequest(client, contextId) =>
        if (store.hasContext(client.clientId, contextId)) {
          val handler =
            context.actorOf(PopContextHandler.props(config, timeout, runtime))
          handler.forward(Api.PopContextRequest(contextId))
        } else {
          sender() ! AccessDenied
        }

      case RecomputeContextRequest(client, contextId, expressions) =>
        if (store.hasContext(client.clientId, contextId)) {
          val handler =
            context.actorOf(
              RecomputeContextHandler.props(config, timeout, runtime)
            )
          val invalidatedExpressions =
            expressions.map(toRuntimeInvalidatedExpressions)
          handler.forward(
            Api.RecomputeContextRequest(contextId, invalidatedExpressions)
          )
        } else {
          sender() ! AccessDenied
        }

      case ExecuteExpression(clientId, visualisationId, expressionId, cfg) =>
        val contextId = cfg.executionContextId
        if (store.hasContext(clientId, contextId)) {
          store.getListener(contextId).foreach { listener =>
            listener ! RegisterOneshotVisualisation(
              contextId,
              visualisationId,
              expressionId
            )
          }
          val handler =
            context.actorOf(
              AttachVisualisationHandler.props(config, timeout, runtime)
            )
          handler.forward(
            Api.AttachVisualisation(
              visualisationId,
              expressionId,
              convertVisualisationConfig(cfg)
            )
          )
        } else {
          sender() ! AccessDenied
        }

      case AttachVisualisation(clientId, visualisationId, expressionId, cfg) =>
        if (store.hasContext(clientId, cfg.executionContextId)) {
          val handler =
            context.actorOf(
              AttachVisualisationHandler.props(config, timeout, runtime)
            )
          handler.forward(
            Api.AttachVisualisation(
              visualisationId,
              expressionId,
              convertVisualisationConfig(cfg)
            )
          )
        } else {
          sender() ! AccessDenied
        }

      case DetachVisualisation(
            clientId,
            contextId,
            visualisationId,
            expressionId
          ) =>
        if (store.hasContext(clientId, contextId)) {
          val handler =
            context.actorOf(
              DetachVisualisationHandler.props(config, timeout, runtime)
            )
          handler.forward(
            Api.DetachVisualisation(contextId, visualisationId, expressionId)
          )
        } else {
          sender() ! AccessDenied
        }

      case ModifyVisualisation(clientId, visualisationId, cfg) =>
        if (store.hasContext(clientId, cfg.executionContextId)) {
          val handler =
            context.actorOf(
              ModifyVisualisationHandler.props(config, timeout, runtime)
            )

          val configuration = convertVisualisationConfig(cfg)

          handler.forward(
            Api.ModifyVisualisation(visualisationId, configuration)
          )
        } else {
          sender() ! AccessDenied
        }
    }

  private def convertVisualisationConfig(
    config: VisualisationConfiguration
  ): Api.VisualisationConfiguration =
    Api.VisualisationConfiguration(
      executionContextId  = config.executionContextId,
      visualisationModule = config.visualisationModule,
      expression          = config.expression
    )

  private def getRuntimeStackItem(
    stackItem: StackItem
  ): Api.StackItem =
    stackItem match {
      case StackItem.ExplicitCall(pointer, argument, arguments) =>
        val methodPointer = getRuntimeMethodPointer(pointer)
        Api.StackItem.ExplicitCall(methodPointer, argument, arguments)

      case StackItem.LocalCall(expressionId) =>
        Api.StackItem.LocalCall(expressionId)
    }

  private def getRuntimeMethodPointer(
    pointer: MethodPointer
  ): Api.MethodPointer =
    Api.MethodPointer(
      module        = pointer.module,
      definedOnType = pointer.definedOnType,
      name          = pointer.name
    )

  private def toRuntimeInvalidatedExpressions(
    expressions: InvalidatedExpressions
  ): Api.InvalidatedExpressions =
    expressions match {
      case InvalidatedExpressions.All =>
        Api.InvalidatedExpressions.All()
      case InvalidatedExpressions.Expressions(es) =>
        Api.InvalidatedExpressions.Expressions(es)
    }

}

object ContextRegistry {

  private case class Store(
    contexts: Map[ClientId, Set[ContextId]],
    listeners: Map[ContextId, ActorRef]
  ) {

    def addContext(
      client: ClientId,
      contextId: ContextId,
      listener: ActorRef
    ): Store =
      copy(
        contexts  = contexts + (client -> (getContexts(client) + contextId)),
        listeners = listeners + (contextId -> listener)
      )

    def removeContext(client: ClientId, contextId: ContextId): Store =
      copy(
        contexts  = contexts.updated(client, getContexts(client) - contextId),
        listeners = listeners - contextId
      )

    def getContexts(client: ClientId): Set[ContextId] =
      contexts.getOrElse(client, Set())

    def getListener(contextId: ContextId): Option[ActorRef] =
      listeners.get(contextId)

    def hasContext(client: ClientId, contextId: ContextId): Boolean =
      getContexts(client).contains(contextId)
  }

  private object Store {

    def apply(): Store =
      Store(Map(), Map())
  }

  /** Creates a configuration object used to create a [[ContextRegistry]].
    *
    * @param repo the suggestions repo
    * @param config language server configuration
    * @param runtime reference to the [[RuntimeConnector]]
    * @param sessionRouter the session router
    */
  def props(
    repo: SuggestionsRepo[Future],
    config: Config,
    runtime: ActorRef,
    sessionRouter: ActorRef
  ): Props =
    Props(new ContextRegistry(repo, config, runtime, sessionRouter))
}
