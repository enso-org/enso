package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.pipe
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.runtime.ContextRegistryProtocol.{
  DetachVisualisation,
  RegisterOneshotVisualisation,
  VisualisationContext,
  VisualisationUpdate
}
import org.enso.languageserver.runtime.ExecutionApi.ContextId
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.session.SessionRouter.{
  DeliverToBinaryController,
  DeliverToJsonController
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.searcher.SuggestionsRepo

import scala.concurrent.Future
import scala.concurrent.duration._

/** EventListener listens event stream for the notifications from the runtime
  * and send updates to the client. The listener is created per context, and
  * only handles the notifications with the given `contextId`.
  *
  * Expression updates are collected and sent to the user in a batch.
  *
  * @param runtimeFailureMapper mapper for runtime failures
  * @param repo the suggestions repo
  * @param rpcSession reference to the client
  * @param contextId exectuion context identifier
  * @param sessionRouter the session router
  * @param updatesSendRate how often send the updates to the user
  */
final class ContextEventsListener(
  runtimeFailureMapper: RuntimeFailureMapper,
  repo: SuggestionsRepo[Future],
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

  def withState(
    oneshotVisualisations: Set[Api.VisualisationContext],
    expressionUpdates: Vector[Api.ExpressionUpdate]
  ): Receive = {

    case RegisterOneshotVisualisation(
          contextId,
          visualisationId,
          expressionId
        ) =>
      val visualisationContext =
        Api.VisualisationContext(
          visualisationId,
          contextId,
          expressionId
        )
      context.become(
        withState(
          oneshotVisualisations + visualisationContext,
          expressionUpdates
        )
      )

    case Api.VisualisationUpdate(ctx, data) if ctx.contextId == contextId =>
      val payload =
        VisualisationUpdate(
          VisualisationContext(
            ctx.visualisationId,
            ctx.contextId,
            ctx.expressionId
          ),
          data
        )
      sessionRouter ! DeliverToBinaryController(rpcSession.clientId, payload)
      if (oneshotVisualisations.contains(ctx)) {
        context.parent ! DetachVisualisation(
          rpcSession.clientId,
          contextId,
          ctx.visualisationId,
          ctx.expressionId
        )
        context.become(
          withState(
            oneshotVisualisations - ctx,
            expressionUpdates
          )
        )
      }

    case Api.ExpressionUpdates(`contextId`, apiUpdates) =>
      context.become(
        withState(oneshotVisualisations, expressionUpdates :++ apiUpdates)
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

    case Api.VisualisationEvaluationFailed(
          `contextId`,
          visualisationId,
          expressionId,
          message,
          diagnostic
        ) =>
      val response = for {
        diagnostic <- diagnostic
          .map(runtimeFailureMapper.toProtocolDiagnostic)
          .sequence
        payload =
          ContextRegistryProtocol.VisualisationEvaluationFailed(
            contextId,
            visualisationId,
            expressionId,
            message,
            diagnostic
          )
      } yield DeliverToJsonController(rpcSession.clientId, payload)

      response.pipeTo(sessionRouter)

    case RunExpressionUpdates if expressionUpdates.nonEmpty =>
      runExpressionUpdates(expressionUpdates)
      context.become(withState(oneshotVisualisations, Vector()))

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
    def toMethodPointer(call: Api.MethodPointer): (String, String, String) =
      (call.module, call.definedOnType, call.name)

    val methodPointerToExpression =
      expressionUpdates.flatMap { update =>
        update.methodCall.map(call =>
          toMethodPointer(call) -> update.expressionId
        )
      }.toMap
    val methodPointers = methodPointerToExpression.keys.toSeq
    repo
      .getAllMethods(methodPointers)
      .map { suggestionIds =>
        val methodPointerToSuggestion =
          methodPointers
            .zip(suggestionIds)
            .collect { case (ptr, Some(suggestionId)) =>
              ptr -> suggestionId
            }
            .toMap
        val computedExpressions = expressionUpdates.map { update =>
          ContextRegistryProtocol.ExpressionUpdate(
            update.expressionId,
            update.expressionType,
            update.methodCall.flatMap { call =>
              val pointer = toMethodPointer(call)
              methodPointerToSuggestion.get(pointer) match {
                case suggestionId @ Some(_) => suggestionId
                case None =>
                  logger.error("Unable to find suggestion for [{}].", pointer)
                  None
              }
            },
            update.profilingInfo.map(toProtocolProfilingInfo),
            update.fromCache,
            toProtocolPayload(update.payload)
          )
        }
        val payload = ContextRegistryProtocol.ExpressionUpdatesNotification(
          contextId,
          computedExpressions
        )
        DeliverToJsonController(rpcSession.clientId, payload)

      }
      .pipeTo(sessionRouter)
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
      case Api.ExpressionUpdate.Payload.Value() =>
        ContextRegistryProtocol.ExpressionUpdate.Payload.Value

      case Api.ExpressionUpdate.Payload.DataflowError(trace) =>
        ContextRegistryProtocol.ExpressionUpdate.Payload.DataflowError(trace)

      case Api.ExpressionUpdate.Payload.Panic(message, trace) =>
        ContextRegistryProtocol.ExpressionUpdate.Payload
          .Panic(message, trace)
    }

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
}

object ContextEventsListener {

  /** The action to process the expression updates. */
  case object RunExpressionUpdates

  /** Creates a configuration object used to create a [[ContextEventsListener]].
    *
    * @param runtimeFailureMapper mapper for runtime failures
    * @param repo the suggestions repo
    * @param rpcSession reference to the client
    * @param contextId exectuion context identifier
    * @param sessionRouter the session router
    * @param updatesSendRate how often send the updates to the user
    */
  def props(
    runtimeFailureMapper: RuntimeFailureMapper,
    repo: SuggestionsRepo[Future],
    rpcSession: JsonSession,
    contextId: ContextId,
    sessionRouter: ActorRef,
    updatesSendRate: FiniteDuration = 1.second
  ): Props =
    Props(
      new ContextEventsListener(
        runtimeFailureMapper,
        repo,
        rpcSession,
        contextId,
        sessionRouter: ActorRef,
        updatesSendRate
      )
    )

}
