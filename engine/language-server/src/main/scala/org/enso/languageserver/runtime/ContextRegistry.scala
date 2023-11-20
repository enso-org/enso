package org.enso.languageserver.runtime

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

import java.util.UUID

import scala.concurrent.duration._

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
  * @param config configuration
  * @param runtimeFailureMapper mapper for runtime failures
  * @param runtime reference to the [[RuntimeConnector]]
  * @param sessionRouter the session router
  */
final class ContextRegistry(
  config: Config,
  runtimeFailureMapper: RuntimeFailureMapper,
  runtime: ActorRef,
  sessionRouter: ActorRef
) extends Actor
    with LazyLogging
    with ActorMessageLogging
    with UnhandledLogging {

  import ContextRegistryProtocol._

  private val timeout: FiniteDuration =
    config.executionContext.requestTimeout

  override def preStart(): Unit = {
    context.system.eventStream
      .subscribe(self, classOf[Api.ExpressionUpdates])
    context.system.eventStream
      .subscribe(self, classOf[Api.VisualizationUpdate])
    context.system.eventStream
      .subscribe(self, classOf[Api.ExecutionFailed])
    context.system.eventStream
      .subscribe(self, classOf[Api.ExecutionComplete])
    context.system.eventStream
      .subscribe(self, classOf[Api.ExecutionUpdate])
    context.system.eventStream
      .subscribe(self, classOf[Api.VisualizationEvaluationFailed])
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

      case update: Api.VisualizationUpdate =>
        store
          .getListener(update.visualizationContext.contextId)
          .foreach(_ ! update)

      case update: Api.ExecutionFailed =>
        store.getListener(update.contextId).foreach(_ ! update)

      case update: Api.ExecutionComplete =>
        store.getListener(update.contextId).foreach(_ ! update)

      case update: Api.ExecutionUpdate =>
        store.getListener(update.contextId).foreach(_ ! update)

      case update: Api.VisualizationExpressionFailed =>
        store.getListener(update.ctx.contextId).foreach(_ ! update)

      case update: Api.VisualizationEvaluationFailed =>
        store.getListener(update.ctx.contextId).foreach(_ ! update)

      case CreateContextRequest(client, contextIdOpt) =>
        val contextId = contextIdOpt.getOrElse(UUID.randomUUID())
        if (!store.hasContext(client.clientId, contextId)) {
          val handler = context.actorOf(
            CreateContextHandler.props(runtimeFailureMapper, timeout, runtime)
          )
          val listener =
            context.actorOf(
              ContextEventsListener.props(
                runtimeFailureMapper,
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
        } else {
          sender() ! CreateContextResponse(contextId)
        }

      case DestroyContextRequest(client, contextId) =>
        if (store.hasContext(client.clientId, contextId)) {
          val handler = context.actorOf(
            DestroyContextHandler.props(
              runtimeFailureMapper,
              timeout,
              runtime
            )
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
            context.actorOf(
              PushContextHandler.props(runtimeFailureMapper, timeout, runtime)
            )
          handler.forward(Api.PushContextRequest(contextId, item))

        } else {
          sender() ! AccessDenied
        }

      case PopContextRequest(client, contextId) =>
        if (store.hasContext(client.clientId, contextId)) {
          val handler = context.actorOf(
            PopContextHandler.props(runtimeFailureMapper, timeout, runtime)
          )
          handler.forward(Api.PopContextRequest(contextId))
        } else {
          sender() ! AccessDenied
        }

      case RecomputeContextRequest(
            client,
            contextId,
            expressions,
            environment
          ) =>
        if (store.hasContext(client.clientId, contextId)) {
          val handler =
            context.actorOf(
              RecomputeContextHandler.props(
                runtimeFailureMapper,
                timeout,
                runtime
              )
            )
          val invalidatedExpressions =
            expressions.map(toRuntimeInvalidatedExpressions)
          handler.forward(
            Api.RecomputeContextRequest(
              contextId,
              invalidatedExpressions,
              environment.map(ExecutionEnvironment.toApi)
            )
          )
        } else {
          sender() ! AccessDenied
        }

      case SetExecutionEnvironmentRequest(client, contextId, environment) =>
        if (store.hasContext(client.clientId, contextId)) {
          val handler =
            context.actorOf(
              SetExecutionContextEnvironmentHandler.props(
                runtimeFailureMapper,
                timeout,
                runtime
              )
            )
          handler.forward(
            Api.SetExecutionEnvironmentRequest(
              contextId,
              ExecutionEnvironment.toApi(environment)
            )
          )
        } else {
          sender() ! AccessDenied
        }

      case InterruptContextRequest(client, contextId) =>
        if (store.hasContext(client.clientId, contextId)) {
          val handler =
            context.actorOf(
              InterruptContextHandler.props(
                runtimeFailureMapper,
                timeout,
                runtime
              )
            )
          handler.forward(Api.InterruptContextRequest(contextId))
        } else {
          sender() ! AccessDenied
        }

      case GetComponentGroupsRequest(clientId, contextId) =>
        if (store.hasContext(clientId, contextId)) {
          val handler =
            context.actorOf(
              GetComponentGroupsHandler.props(
                runtimeFailureMapper,
                timeout,
                runtime
              )
            )
          handler.forward(Api.GetComponentGroupsRequest())
        } else {
          sender() ! AccessDenied
        }

      case ExecuteExpression(
            clientId,
            contextId,
            visualizationId,
            expressionId,
            expression
          ) =>
        if (store.hasContext(clientId, contextId)) {
          store.getListener(contextId).foreach { listener =>
            listener ! RegisterOneshotVisualization(
              contextId,
              visualizationId,
              expressionId
            )
          }
          val handler = context.actorOf(
            ExecuteExpressionHandler.props(
              runtimeFailureMapper,
              timeout,
              runtime
            )
          )
          handler.forward(
            Api.ExecuteExpression(
              contextId,
              visualizationId,
              expressionId,
              expression
            )
          )
        } else {
          sender() ! AccessDenied
        }

      case AttachVisualization(clientId, visualizationId, expressionId, cfg) =>
        if (store.hasContext(clientId, cfg.executionContextId)) {
          val handler = context.actorOf(
            AttachVisualizationHandler.props(
              runtimeFailureMapper,
              timeout,
              runtime
            )
          )
          handler.forward(
            Api.AttachVisualization(
              visualizationId,
              expressionId,
              cfg.toApi
            )
          )
        } else {
          sender() ! AccessDenied
        }

      case DetachVisualization(
            clientId,
            contextId,
            visualizationId,
            expressionId
          ) =>
        if (store.hasContext(clientId, contextId)) {
          val handler = context.actorOf(
            DetachVisualizationHandler.props(
              runtimeFailureMapper,
              timeout,
              runtime
            )
          )
          handler.forward(
            Api.DetachVisualization(contextId, visualizationId, expressionId)
          )
        } else {
          sender() ! AccessDenied
        }

      case ModifyVisualization(clientId, visualizationId, cfg) =>
        if (store.hasContext(clientId, cfg.executionContextId)) {
          val handler = context.actorOf(
            ModifyVisualizationHandler.props(
              runtimeFailureMapper,
              timeout,
              runtime
            )
          )

          handler.forward(
            Api.ModifyVisualization(visualizationId, cfg.toApi)
          )
        } else {
          sender() ! AccessDenied
        }
    }

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

    def getListener(contextId: ContextId): Option[ActorRef] =
      listeners.get(contextId)

    def hasContext(client: ClientId, contextId: ContextId): Boolean =
      getContexts(client).contains(contextId)

    private def getContexts(client: ClientId): Set[ContextId] =
      contexts.getOrElse(client, Set())
  }

  private object Store {

    def apply(): Store =
      Store(Map(), Map())
  }

  /** Creates a configuration object used to create a [[ContextRegistry]].
    *
    * @param config language server configuration
    * @param runtimeFailureMapper mapper for runtime failures
    * @param runtime reference to the [[RuntimeConnector]]
    * @param sessionRouter the session router
    */
  def props(
    config: Config,
    runtimeFailureMapper: RuntimeFailureMapper,
    runtime: ActorRef,
    sessionRouter: ActorRef
  ): Props =
    Props(
      new ContextRegistry(
        config,
        runtimeFailureMapper,
        runtime,
        sessionRouter
      )
    )
}
