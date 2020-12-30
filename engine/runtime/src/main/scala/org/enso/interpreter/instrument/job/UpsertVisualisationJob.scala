package org.enso.interpreter.instrument.job

import cats.implicits._
import org.enso.interpreter.instrument.Visualisation
import org.enso.interpreter.instrument.execution.{Executable, RuntimeContext}
import org.enso.interpreter.instrument.job.UpsertVisualisationJob.{
  EvalFailure,
  EvaluationFailed,
  ModuleNotFound
}
import org.enso.polyglot.runtime.Runtime.Api.{
  ExpressionId,
  RequestId,
  VisualisationId
}
import org.enso.polyglot.runtime.Runtime.{Api, ApiResponse}

import scala.util.control.NonFatal

/**
  * A job that upserts a visualisation.
  *
  * @param requestId maybe a request id
  * @param visualisationId an identifier of visualisation
  * @param expressionId an identifier of expression
  * @param config a visualisation config
  * @param response a response used to reply to a client
  */
class UpsertVisualisationJob(
  requestId: Option[RequestId],
  visualisationId: VisualisationId,
  expressionId: ExpressionId,
  config: Api.VisualisationConfiguration,
  response: ApiResponse
) extends Job[Option[Executable]](
      List(config.executionContextId),
      true,
      false
    ) {

  /** @inheritdoc **/
  override def run(implicit ctx: RuntimeContext): Option[Executable] = {
    ctx.locking.acquireContextLock(config.executionContextId)
    ctx.locking.acquireWriteCompilationLock()
    try {
      val maybeCallable =
        evaluateExpression(config.visualisationModule, config.expression)

      maybeCallable match {
        case Left(ModuleNotFound) =>
          replyWithModuleNotFoundError()
          None

        case Left(EvaluationFailed(msg)) =>
          replyWithExpressionFailedError(msg)
          None

        case Right(callable) =>
          updateVisualisation(callable)
          ctx.endpoint.sendToClient(Api.Response(requestId, response))
          val stack = ctx.contextManager.getStack(config.executionContextId)
          val exe =
            Executable(config.executionContextId, stack, Seq(expressionId))
          Some(exe)
      }
    } finally {
      ctx.locking.releaseWriteCompilationLock()
      ctx.locking.releaseContextLock(config.executionContextId)
    }

  }

  private def updateVisualisation(
    callable: AnyRef
  )(implicit ctx: RuntimeContext): Unit = {
    val visualisation = Visualisation(
      visualisationId,
      expressionId,
      callable
    )
    ctx.contextManager.upsertVisualisation(
      config.executionContextId,
      visualisation
    )
  }

  private def replyWithExpressionFailedError(
    msg: String
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.endpoint.sendToClient(
      Api.Response(
        requestId,
        Api.VisualisationExpressionFailed(msg)
      )
    )
  }

  private def replyWithModuleNotFoundError()(
    implicit ctx: RuntimeContext
  ): Unit = {
    ctx.endpoint.sendToClient(
      Api.Response(
        requestId,
        Api.ModuleNotFound(config.visualisationModule)
      )
    )
  }

  private def evaluateExpression(
    moduleName: String,
    expression: String
  )(implicit ctx: RuntimeContext): Either[EvalFailure, AnyRef] = {
    val maybeModule = ctx.executionService.findModule(moduleName)

    val notFoundOrModule =
      if (maybeModule.isPresent) Right(maybeModule.get())
      else Left(ModuleNotFound)

    notFoundOrModule.flatMap { module =>
      try {
        ctx.executionService.evaluateExpression(module, expression).asRight
      } catch {
        case NonFatal(th) => EvaluationFailed(th.getMessage).asLeft
      }
    }

  }

}

object UpsertVisualisationJob {

  /**
    * Base trait for evaluation failures.
    */
  sealed trait EvalFailure

  /**
    * Signals that a module cannot be found.
    */
  case object ModuleNotFound extends EvalFailure

  /**
    * Signals that an evaluation of an expression failed.
    *
    * @param msg the textual reason of a failure
    */
  case class EvaluationFailed(msg: String) extends EvalFailure

}
