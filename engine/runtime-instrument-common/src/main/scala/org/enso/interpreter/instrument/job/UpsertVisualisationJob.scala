package org.enso.interpreter.instrument.job

import cats.implicits._
import org.enso.compiler.core.IR
import org.enso.compiler.pass.analyse.{
  CachePreferenceAnalysis,
  DataflowAnalysis
}
import org.enso.interpreter.instrument.execution.{Executable, RuntimeContext}
import org.enso.interpreter.instrument.job.UpsertVisualisationJob.{
  EvaluationFailed,
  EvaluationResult,
  ModuleNotFound
}
import org.enso.interpreter.instrument.{
  CacheInvalidation,
  InstrumentFrame,
  RuntimeCache,
  Visualisation
}
import org.enso.interpreter.runtime.Module
import org.enso.interpreter.runtime.control.ThreadInterruptedException
import org.enso.pkg.QualifiedName
import org.enso.polyglot.runtime.Runtime.Api._
import org.enso.polyglot.runtime.Runtime.{Api, ApiResponse}

import java.util.logging.Level

/** A job that upserts a visualisation.
  *
  * @param requestId maybe a request id
  * @param response a response used to reply to a client
  * @param visualisationId an identifier of visualisation
  * @param expressionId an identifier of expression
  * @param config a visualisation config
  */
class UpsertVisualisationJob(
  requestId: Option[RequestId],
  response: ApiResponse,
  visualisationId: VisualisationId,
  expressionId: ExpressionId,
  config: Api.VisualisationConfiguration
) extends UniqueJob[Option[Executable]](
      expressionId,
      List(config.executionContextId),
      false
    ) {

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Option[Executable] = {
    ctx.locking.acquireContextLock(config.executionContextId)
    ctx.locking.acquireWriteCompilationLock()
    try {
      val maybeCallable =
        UpsertVisualisationJob.evaluateVisualisationExpression(
          config.expression
        )

      maybeCallable match {
        case Left(ModuleNotFound(moduleName)) =>
          replyWithError(Api.ModuleNotFound(moduleName))
          None

        case Left(EvaluationFailed(message, result)) =>
          replyWithExpressionFailedError(message, result)
          None

        case Right(EvaluationResult(module, callable, arguments)) =>
          val visualisation =
            UpsertVisualisationJob.updateVisualisation(
              visualisationId,
              expressionId,
              module,
              config,
              callable,
              arguments
            )
          ctx.endpoint.sendToClient(Api.Response(requestId, response))
          val stack = ctx.contextManager.getStack(config.executionContextId)
          val cachedValue = stack.headOption
            .flatMap(frame => Option(frame.cache.get(expressionId)))
          UpsertVisualisationJob.requireVisualisationSynchronization(
            stack,
            expressionId
          )
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

  private def replyWithError(error: Api.Error)(implicit
    ctx: RuntimeContext
  ): Unit = {
    ctx.endpoint.sendToClient(Api.Response(requestId, error))
  }
}

object UpsertVisualisationJob {

  /** The number of times to retry the expression evaluation. */
  val MaxEvaluationRetryCount: Int = 5

  /** Base trait for evaluation failures.
    */
  sealed trait EvaluationFailure

  /** Signals that a module cannot be found.
    *
    * @param moduleName the module name
    */
  case class ModuleNotFound(moduleName: String) extends EvaluationFailure

  /** Signals that an evaluation of an expression failed.
    *
    * @param message the textual reason of a failure
    * @param failure the error description
    */
  case class EvaluationFailed(
    message: String,
    failure: Option[Api.ExecutionResult.Diagnostic]
  ) extends EvaluationFailure

  /** The result of evaluating the method pointer and positional argument
    * expressions.
    *
    * @param module the resolved module
    * @param callback the Enso function
    * @param arguments the list of arguments that will be passed to the callback
    */
  case class EvaluationResult(
    module: Module,
    callback: AnyRef,
    arguments: Vector[AnyRef]
  )

  /** Upsert the provided visualisation.
    *
    * @param visualisation the visualisation to update
    * @param ctx the runtime context
    */
  def upsertVisualisation(
    visualisation: Visualisation
  )(implicit ctx: RuntimeContext): Unit = {
    val visualisationConfig = visualisation.config
    val expressionId        = visualisation.expressionId
    val visualisationId     = visualisation.id
    val maybeCallable =
      evaluateVisualisationExpression(visualisation.config.expression)

    maybeCallable.foreach { result =>
      updateVisualisation(
        visualisationId,
        expressionId,
        result.module,
        visualisationConfig,
        result.callback,
        result.arguments
      )
      val stack =
        ctx.contextManager.getStack(visualisationConfig.executionContextId)
      requireVisualisationSynchronization(stack, expressionId)
    }
  }

  /** Find module by name.
    *
    * @param moduleName the module name
    * @return either the requested module or an error
    */
  private def findModule(
    moduleName: String
  )(implicit ctx: RuntimeContext): Either[EvaluationFailure, Module] = {
    val context = ctx.executionService.getContext
    context.ensureModuleIsLoaded(moduleName)
    val maybeModule = context.findModule(moduleName)

    if (maybeModule.isPresent) Right(maybeModule.get())
    else Left(ModuleNotFound(moduleName))
  }

  /** Evaluate the visualisation expression in a given module.
    *
    * @param module the module where to evaluate the expression
    * @param expression the visualisation expression
    * @param retryCount the number of attempted retries
    * @param ctx the runtime context
    * @return either the evaluation result or an evaluation failure
    */
  private def evaluateModuleExpression(
    module: Module,
    expression: Api.VisualisationExpression,
    retryCount: Int = 0
  )(implicit
    ctx: RuntimeContext
  ): Either[EvaluationFailure, EvaluationResult] =
    Either
      .catchNonFatal {
        val (callback, arguments) = expression match {
          case Api.VisualisationExpression.Text(_, expression) =>
            val callback = ctx.executionService.evaluateExpression(
              module,
              expression
            )
            val arguments = Vector()
            (callback, arguments)
          case Api.VisualisationExpression.ModuleMethod(
                Api.MethodPointer(_, definedOnType, name),
                argumentExpressions
              ) =>
            val callback = ctx.executionService.prepareFunctionCall(
              module,
              QualifiedName.fromString(definedOnType).item,
              name
            )
            val arguments = argumentExpressions.map(
              ctx.executionService.evaluateExpression(module, _)
            )
            (callback, arguments)
        }
        EvaluationResult(module, callback, arguments)
      }
      .leftFlatMap {
        case _: ThreadInterruptedException
            if retryCount < MaxEvaluationRetryCount =>
          ctx.executionService.getLogger.log(
            Level.FINE,
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

  /** Evaluate the visualisation expression.
    *
    * @param expression the visualisation expression to evaluate
    * @param ctx the runtime context
    * @return either the evaluation result or an evaluation error
    */
  private def evaluateVisualisationExpression(
    expression: Api.VisualisationExpression
  )(implicit
    ctx: RuntimeContext
  ): Either[EvaluationFailure, EvaluationResult] = {
    for {
      module     <- findModule(expression.module)
      expression <- evaluateModuleExpression(module, expression)
    } yield expression
  }

  /** Update the visualisation state.
    *
    * @param visualisationId the visualisation identifier
    * @param expressionId the expression to which the visualisation is applied
    * @param module the module containing the visualisation
    * @param visualisationConfig the visualisation configuration
    * @param callback the visualisation callback function
    * @param arguments the list of arugments that will be passed to the callback
    * @param ctx the runtime context
    * @return the re-evaluated visualisation
    */
  private def updateVisualisation(
    visualisationId: VisualisationId,
    expressionId: ExpressionId,
    module: Module,
    visualisationConfig: VisualisationConfiguration,
    callback: AnyRef,
    arguments: Vector[AnyRef]
  )(implicit ctx: RuntimeContext): Visualisation = {
    val visualisationExpressionId =
      findVisualisationExpressionId(module, visualisationConfig.expression)
    val visualisation = Visualisation(
      visualisationId,
      expressionId,
      new RuntimeCache(),
      module,
      visualisationConfig,
      visualisationExpressionId,
      callback,
      arguments
    )
    invalidateCaches(visualisation)
    ctx.contextManager.upsertVisualisation(
      visualisationConfig.executionContextId,
      visualisation
    )
    visualisation
  }

  /** Find the expressionId of visualisation function.
    *
    * @param module the module environment
    * @param visualisationExpression the visualisation expression
    * @return the expression id of required visualisation function
    */
  private def findVisualisationExpressionId(
    module: Module,
    visualisationExpression: VisualisationExpression
  ): Option[ExpressionId] =
    visualisationExpression match {
      case VisualisationExpression.ModuleMethod(methodPointer, _) =>
        module.getIr.bindings
          .collect { case method: IR.Module.Scope.Definition.Method =>
            val methodReference        = method.methodReference
            val methodReferenceName    = methodReference.methodName.name
            val methodReferenceTypeOpt = methodReference.typePointer.map(_.name)

            val externalIdOpt = method.body match {
              case fun: IR.Function => fun.body.getExternalId
              case _                => method.getExternalId
            }
            externalIdOpt.filter { _ =>
              methodReferenceName == methodPointer.name &&
              methodReferenceTypeOpt.isEmpty
            }
          }
          .flatten
          .headOption

      case _: VisualisationExpression.Text => None
    }

  /** Update the caches. */
  private def invalidateCaches(
    visualisation: Visualisation
  )(implicit ctx: RuntimeContext): Unit = {
    setCacheWeights(visualisation)
    val stacks = ctx.contextManager.getAllContexts.values
    /* The invalidation of the first cached dependent node is required for
     * attaching the visualizations to sub-expressions. Consider the example
     * ```
     * op = target.foo arg
     * ```
     * The result of expression `target.foo arg` is cached. If you attach the
     * visualization to say `target`, the sub-expression `target` won't be
     * executed because the whole expression is cached. And the visualization
     * won't be computed.
     * To workaround this issue, the logic below tries to identify if the
     * visualized expression is a sub-expression and invalidate the first parent
     * expression accordingly.
     */
    if (!stacks.exists(isExpressionCached(visualisation.expressionId, _))) {
      invalidateFirstDependent(visualisation.expressionId)
    }
  }

  /** Check if the expression is cached in the execution stack.
    *
    * @param expressionId the expression id to check
    * @param stack the execution stack
    * @return `true` if the expression exists in the frame cache
    */
  private def isExpressionCached(
    expressionId: ExpressionId,
    stack: Iterable[InstrumentFrame]
  ): Boolean = {
    stack.headOption.exists { frame =>
      frame.cache.get(expressionId) ne null
    }
  }

  /** Set the cache weights for the provided visualisation.
    *
    * @param visualisation the visualisation to update
    */
  private def setCacheWeights(visualisation: Visualisation): Unit = {
    visualisation.module.getIr.getMetadata(CachePreferenceAnalysis).foreach {
      metadata =>
        CacheInvalidation.runVisualisations(
          Seq(visualisation),
          CacheInvalidation.Command.SetMetadata(metadata)
        )
    }
  }

  /** Invalidate the first cached dependent node of the provided expression.
    *
    * @param expressionId the expression id
    */
  private def invalidateFirstDependent(
    expressionId: ExpressionId
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.executionService.getContext
      .findModuleByExpressionId(expressionId)
      .ifPresent { module =>
        module.getIr
          .getMetadata(DataflowAnalysis)
          .foreach { metadata =>
            module.getIr.preorder
              .find(_.getExternalId.contains(expressionId))
              .collect {
                case name: IR.Name =>
                  DataflowAnalysis.DependencyInfo.Type
                    .Dynamic(name.name, Some(expressionId))
                case ir =>
                  DataflowAnalysis.DependencyInfo.Type
                    .Static(ir.getId, ir.getExternalId)
              }
              .flatMap { expressionKey =>
                metadata.dependents.getExternal(expressionKey)
              }
              .foreach { dependents =>
                val stacks = ctx.contextManager.getAllContexts.values
                stacks.foreach { stack =>
                  stack.headOption.foreach { frame =>
                    dependents
                      .find { id => frame.cache.get(id) ne null }
                      .foreach { firstDependent =>
                        CacheInvalidation.run(
                          stack,
                          CacheInvalidation(
                            CacheInvalidation.StackSelector.Top,
                            CacheInvalidation.Command
                              .InvalidateKeys(Seq(firstDependent))
                          )
                        )
                      }
                  }
                }
              }
          }
      }
  }

  /** Require to send the visualisation update.
    *
    * @param stack the execution stack
    * @param expressionId the expression id to which the visualisation is applied
    */
  private def requireVisualisationSynchronization(
    stack: Iterable[InstrumentFrame],
    expressionId: ExpressionId
  ): Unit =
    stack.foreach(_.syncState.setVisualisationUnsync(expressionId))

}
