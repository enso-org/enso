package org.enso.interpreter.instrument.job

import java.util.logging.Level
import cats.implicits._
import org.enso.interpreter.instrument.{
  InstrumentFrame,
  RuntimeCache,
  Visualisation
}
import org.enso.interpreter.instrument.execution.{Executable, RuntimeContext}
import org.enso.interpreter.instrument.job.UpsertVisualisationJob.{
  EvalFailure,
  EvaluationFailed,
  MaxEvaluationRetryCount,
  ModuleNotFound
}
import org.enso.interpreter.runtime.Module
import org.enso.interpreter.runtime.control.ThreadInterruptedException
import org.enso.pkg.QualifiedName
import org.enso.polyglot.runtime.Runtime.Api.{
  ExpressionId,
  RequestId,
  VisualisationId
}
import org.enso.polyglot.runtime.Runtime.{Api, ApiResponse}

/** A job that upserts a visualisation.
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

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Option[Executable] = {
    ctx.locking.acquireContextLock(config.executionContextId)
    ctx.locking.acquireWriteCompilationLock()
    try {
      val maybeCallable =
        evaluateVisualisationExpression(config.expression)

      maybeCallable match {
        case Left(ModuleNotFound) =>
          replyWithModuleNotFoundError(config.expression.module)
          None

        case Left(EvaluationFailed(message, result)) =>
          replyWithExpressionFailedError(message, result)
          None

        case Right(callable) =>
          val visualisation = updateVisualisation(callable)
          ctx.endpoint.sendToClient(Api.Response(requestId, response))
          val stack = ctx.contextManager.getStack(config.executionContextId)
          val cachedValue = stack.headOption
            .flatMap(frame => Option(frame.cache.get(expressionId)))
          requireVisualisationSynchronization(stack, expressionId)
          cachedValue match {
            case Some(value) =>
              ProgramExecutionSupport.sendVisualisationUpdate(
                config.executionContextId,
                stack.headOption.get.syncState,
                visualisation,
                expressionId,
                value
              )
              None
            case None =>
              Some(Executable(config.executionContextId, stack))
          }
      }
    } finally {
      ctx.locking.releaseWriteCompilationLock()
      ctx.locking.releaseContextLock(config.executionContextId)
    }
  }

  private def requireVisualisationSynchronization(
    stack: Iterable[InstrumentFrame],
    expressionId: ExpressionId
  ): Unit = {
    stack.foreach(_.syncState.setVisualisationUnsync(expressionId))
  }

  private def updateVisualisation(
    callable: AnyRef
  )(implicit ctx: RuntimeContext): Visualisation = {
    val visualisation = Visualisation(
      visualisationId,
      expressionId,
      RuntimeCache.visualizationCache(),
      callable
    )
    ctx.contextManager.upsertVisualisation(
      config.executionContextId,
      visualisation
    )
    visualisation
  }

  private def replyWithExpressionFailedError(
    message: String,
    executionResult: Option[Api.ExecutionResult.Diagnostic]
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.endpoint.sendToClient(
      Api.Response(
        requestId,
        Api.VisualisationExpressionFailed(message, executionResult)
      )
    )
  }

  private def replyWithModuleNotFoundError(module: String)(implicit
    ctx: RuntimeContext
  ): Unit = {
    ctx.endpoint.sendToClient(
      Api.Response(requestId, Api.ModuleNotFound(module))
    )
  }

  private def findModule(
    moduleName: String
  )(implicit ctx: RuntimeContext): Either[EvalFailure, Module] = {
    val context = ctx.executionService.getContext
    // TODO [RW] more specific error when the module cannot be installed (#1861)
    context.ensureModuleIsLoaded(moduleName)
    val maybeModule = context.findModule(moduleName)

    if (maybeModule.isPresent) Right(maybeModule.get())
    else Left(ModuleNotFound)
  }

  private def evaluateModuleExpression(
    module: Module,
    expression: Api.VisualisationExpression,
    retryCount: Int = 0
  )(implicit
    ctx: RuntimeContext
  ): Either[EvalFailure, AnyRef] =
    Either
      .catchNonFatal {
        expression match {
          case Api.VisualisationExpression.Text(_, expression) =>
            ctx.executionService.evaluateExpression(module, expression)
          case Api.VisualisationExpression.ModuleMethod(
                Api.MethodPointer(_, definedOnType, name)
              ) =>
            ctx.executionService.prepareFunctionCall(
              module,
              QualifiedName.fromString(definedOnType).item,
              name
            )
        }
      }
      .leftFlatMap {
        case _: ThreadInterruptedException
            if retryCount < MaxEvaluationRetryCount =>
          ctx.executionService.getLogger.log(
            Level.WARNING,
            s"Evaluation of visualisation was interrupted. Retrying [${retryCount + 1}]."
          )
          evaluateModuleExpression(module, expression, retryCount + 1)

        case error: ThreadInterruptedException =>
          val message =
            s"Evaluation of visualization failed after [$retryCount] times " +
            s"[${error.getClass.getSimpleName}]."
          ctx.executionService.getLogger
            .log(
              Level.SEVERE,
              message
            )
          Left(
            EvaluationFailed(
              message,
              ProgramExecutionSupport.getDiagnosticOutcome.lift(error)
            )
          )

        case error =>
          ctx.executionService.getLogger
            .log(
              Level.SEVERE,
              "Evaluation of visualization failed: " +
              s"[${error.getClass}] ${error.getMessage}",
              error
            )
          Left(
            EvaluationFailed(
              Option(error.getMessage).getOrElse(error.getClass.getSimpleName),
              ProgramExecutionSupport.getDiagnosticOutcome.lift(error)
            )
          )
      }

  private def evaluateVisualisationExpression(
    expression: Api.VisualisationExpression
  )(implicit ctx: RuntimeContext): Either[EvalFailure, AnyRef] = {
    for {
      module     <- findModule(expression.module)
      expression <- evaluateModuleExpression(module, expression)
    } yield expression
  }
}

object UpsertVisualisationJob {

  /** The number of times to retry the expression evaluation. */
  val MaxEvaluationRetryCount: Int = 5

  /** Base trait for evaluation failures.
    */
  sealed trait EvalFailure

  /** Signals that a module cannot be found.
    */
  case object ModuleNotFound extends EvalFailure

  /** Signals that an evaluation of an expression failed.
    *
    * @param message the textual reason of a failure
    * @param failure the error description
    */
  case class EvaluationFailed(
    message: String,
    failure: Option[Api.ExecutionResult.Diagnostic]
  ) extends EvalFailure

}
