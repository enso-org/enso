package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.pipe
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.runtime.ContextRegistryProtocol.{
  DetachVisualization,
  RegisterOneshotVisualization,
  VisualizationContext,
  VisualizationUpdate
}
import org.enso.languageserver.runtime.ExecutionApi.ContextId
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.session.SessionRouter.{
  DeliverToBinaryController,
  DeliverToJsonController
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.duration._

/** EventListener listens event stream for the notifications from the runtime
  * and send updates to the client. The listener is created per context, and
  * only handles the notifications with the given `contextId`.
  *
  * Expression updates are collected and sent to the user in a batch.
  *
  * @param runtimeFailureMapper mapper for runtime failures
  * @param rpcSession reference to the client
  * @param contextId exectuion context identifier
  * @param sessionRouter the session router
  * @param updatesSendRate how often send the updates to the user
  */
final class ContextEventsListener(
  runtimeFailureMapper: RuntimeFailureMapper,
  rpcSession: JsonSession,
  contextId: ContextId,
  sessionRouter: ActorRef,
  updatesSendRate: FiniteDuration
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import ContextEventsListener.RunExpressionUpdates
  import context.dispatcher

  override def preStart(): Unit = {
    if (updatesSendRate.length > 0) {
      context.system.scheduler.scheduleWithFixedDelay(
        updatesSendRate,
        updatesSendRate,
        self,
        RunExpressionUpdates
      )
    }
  }

  override def receive: Receive = withState(Set(), Vector())

  private def withState(
    oneshotVisualizations: Set[Api.VisualizationContext],
    expressionUpdates: Vector[Api.ExpressionUpdate]
  ): Receive = {

    case RegisterOneshotVisualization(
          contextId,
          visualizationId,
          expressionId
        ) =>
      val visualizationContext =
        Api.VisualizationContext(
          visualizationId,
          contextId,
          expressionId
        )
      context.become(
        withState(
          oneshotVisualizations + visualizationContext,
          expressionUpdates
        )
      )

    case Api.VisualizationUpdate(ctx, data) if ctx.contextId == contextId =>
      val payload =
        VisualizationUpdate(
          VisualizationContext(
            ctx.visualizationId,
            ctx.contextId,
            ctx.expressionId
          ),
          data
        )
      sessionRouter ! DeliverToBinaryController(rpcSession.clientId, payload)
      if (oneshotVisualizations.contains(ctx)) {
        context.parent ! DetachVisualization(
          rpcSession.clientId,
          contextId,
          ctx.visualizationId,
          ctx.expressionId
        )
        context.become(
          withState(
            oneshotVisualizations - ctx,
            expressionUpdates
          )
        )
      }

    case Api.ExpressionUpdates(`contextId`, apiUpdates) =>
      context.become(
        withState(oneshotVisualizations, expressionUpdates :++ apiUpdates)
      )

    case Api.ExecutionFailed(`contextId`, error) =>
      val message = for {
        failure <- runtimeFailureMapper.toProtocolFailure(error)
        payload = ContextRegistryProtocol.ExecutionFailedNotification(
          contextId,
          failure
        )
      } yield DeliverToJsonController(rpcSession.clientId, payload)

      message.pipeTo(sessionRouter)

    case Api.ExecutionComplete(`contextId`) =>
      val payload =
        ContextRegistryProtocol.ExecutionCompleteNotification(contextId)

      sessionRouter ! DeliverToJsonController(rpcSession.clientId, payload)

    case Api.ExecutionUpdate(`contextId`, diagnostics) =>
      val message = for {
        diagnostics <- diagnostics
          .map(runtimeFailureMapper.toProtocolDiagnostic)
          .toList
          .sequence
        payload = ContextRegistryProtocol.ExecutionDiagnosticNotification(
          contextId,
          diagnostics
        )
      } yield DeliverToJsonController(rpcSession.clientId, payload)

      message.pipeTo(sessionRouter)

    case Api.VisualizationEvaluationFailed(
          `contextId`,
          visualizationId,
          expressionId,
          message,
          diagnostic
        ) =>
      val response = for {
        diagnostic <- diagnostic
          .map(runtimeFailureMapper.toProtocolDiagnostic)
          .sequence
        payload =
          ContextRegistryProtocol.VisualizationEvaluationFailed(
            contextId,
            visualizationId,
            expressionId,
            message,
            diagnostic
          )
      } yield DeliverToJsonController(rpcSession.clientId, payload)

      response.pipeTo(sessionRouter)

    case RunExpressionUpdates if expressionUpdates.nonEmpty =>
      runExpressionUpdates(expressionUpdates)
      context.become(withState(oneshotVisualizations, Vector()))

    case RunExpressionUpdates if expressionUpdates.isEmpty =>
  }

  /** Process `ExpressionUpdate` notifications.
    *
    * Function resolves method pointers to the corresponding suggestion ids in
    * the suggestions database, and creates the API updates.
    */
  private def runExpressionUpdates(
    expressionUpdates: Vector[Api.ExpressionUpdate]
  ): Unit = {
    val computedExpressions = expressionUpdates.map { update =>
      ContextRegistryProtocol.ExpressionUpdate(
        update.expressionId,
        update.expressionType,
        update.methodCall.map(toProtocolMethodCall),
        update.profilingInfo.map(toProtocolProfilingInfo),
        update.fromCache,
        toProtocolPayload(update.payload)
      )
    }
    val payload = ContextRegistryProtocol.ExpressionUpdatesNotification(
      contextId,
      computedExpressions
    )
    sessionRouter ! DeliverToJsonController(rpcSession.clientId, payload)
  }

  /** Convert the runtime expression update payload to the context registry
    * protocol representation.
    *
    * @param payload the runtime payload
    * @return the registry protocol representation of the payload message
    */
  private def toProtocolPayload(
    payload: Api.ExpressionUpdate.Payload
  ): ContextRegistryProtocol.ExpressionUpdate.Payload =
    payload match {
      case Api.ExpressionUpdate.Payload.Value(warnings) =>
        ContextRegistryProtocol.ExpressionUpdate.Payload.Value(
          warnings.map(toProtocolWarnings)
        )

      case Api.ExpressionUpdate.Payload.Pending(m, p) =>
        ContextRegistryProtocol.ExpressionUpdate.Payload.Pending(m, p)

      case Api.ExpressionUpdate.Payload.DataflowError(trace) =>
        ContextRegistryProtocol.ExpressionUpdate.Payload.DataflowError(trace)

      case Api.ExpressionUpdate.Payload.Panic(message, trace) =>
        ContextRegistryProtocol.ExpressionUpdate.Payload
          .Panic(message, trace)
    }

  /** Convert the runtime warnings to the context registry protocol
    * representation
    *
    * @param payload the warnings payload
    */
  private def toProtocolWarnings(
    payload: Api.ExpressionUpdate.Payload.Value.Warnings
  ): ContextRegistryProtocol.ExpressionUpdate.Payload.Value.Warnings =
    ContextRegistryProtocol.ExpressionUpdate.Payload.Value
      .Warnings(payload.count, payload.warning, payload.reachedMaxCount)

  /** Convert the runtime profiling info to the context registry protocol
    * representation.
    *
    * @param info the profiling info
    * @return the registry protocol representation of the profiling info
    */
  private def toProtocolProfilingInfo(info: Api.ProfilingInfo): ProfilingInfo =
    info match {
      case Api.ProfilingInfo.ExecutionTime(t) =>
        ProfilingInfo.ExecutionTime(t)
    }

  /** Convert the runtime method call to the context registry protocol
    * representation.
    *
    * @param methodCall the method call
    * @return the registry protocol representation of the method call
    */
  private def toProtocolMethodCall(methodCall: Api.MethodCall): MethodCall =
    MethodCall(
      toProtocolMethodPointer(methodCall.methodPointer),
      methodCall.notAppliedArguments
    )

  /** Convert the runtime method pointer to the context registry protocol
    * representation.
    *
    * @param methodPointer the method pointer
    * @return the registry protocol representation of the method pointer
    */
  private def toProtocolMethodPointer(
    methodPointer: Api.MethodPointer
  ): MethodPointer =
    MethodPointer(
      methodPointer.module,
      methodPointer.definedOnType,
      methodPointer.name
    )
}

object ContextEventsListener {

  /** The action to process the expression updates. */
  case object RunExpressionUpdates

  /** Creates a configuration object used to create a [[ContextEventsListener]].
    *
    * @param runtimeFailureMapper mapper for runtime failures
    * @param rpcSession reference to the client
    * @param contextId exectuion context identifier
    * @param sessionRouter the session router
    * @param updatesSendRate how often send the updates to the user
    */
  def props(
    runtimeFailureMapper: RuntimeFailureMapper,
    rpcSession: JsonSession,
    contextId: ContextId,
    sessionRouter: ActorRef,
    updatesSendRate: FiniteDuration = 1.second
  ): Props =
    Props(
      new ContextEventsListener(
        runtimeFailureMapper,
        rpcSession,
        contextId,
        sessionRouter: ActorRef,
        updatesSendRate
      )
    )

}
