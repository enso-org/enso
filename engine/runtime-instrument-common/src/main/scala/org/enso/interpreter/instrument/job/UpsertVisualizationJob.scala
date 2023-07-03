package org.enso.interpreter.instrument.job

import cats.implicits._
import com.oracle.truffle.api.TruffleLogger
import org.enso.compiler.core.IR
import org.enso.compiler.pass.analyse.{
  CachePreferenceAnalysis,
  DataflowAnalysis
}
import org.enso.interpreter.instrument.execution.{Executable, RuntimeContext}
import org.enso.interpreter.instrument.job.UpsertVisualizationJob.{
  EvaluationFailed,
  EvaluationResult,
  ModuleNotFound
}
import org.enso.interpreter.instrument.{
  CacheInvalidation,
  InstrumentFrame,
  RuntimeCache,
  Visualization
}
import org.enso.interpreter.runtime.Module
import org.enso.interpreter.runtime.control.ThreadInterruptedException
import org.enso.pkg.QualifiedName
//import org.enso.polyglot.runtime.Runtime.Api.
import org.enso.polyglot.runtime.Runtime.Api

import java.util.logging.Level

/** A job that upserts a visualization.
  *
  * @param requestId maybe a request id
  * @param visualizationId an identifier of visualization
  * @param expressionId an identifier of expression
  * @param config a visualization config
  */
class UpsertVisualizationJob(
  requestId: Option[Api.RequestId],
  visualizationId: Api.VisualizationId,
  expressionId: Api.ExpressionId,
  config: Api.VisualizationConfiguration
) extends UniqueJob[Option[Executable]](
      expressionId,
      List(config.executionContextId),
      false
    ) {

  /** Return the id of the visualization associated with this job
    *
    * @return visualization id
    */
  def getVisualizationId(): Api.VisualizationId = visualizationId

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Option[Executable] = {
    implicit val logger = ctx.executionService.getLogger
    val lockTimestamp =
      ctx.locking.acquireContextLock(config.executionContextId)
    try {
      val maybeCallable =
        UpsertVisualizationJob.evaluateVisualizationExpression(
          config.visualizationModule,
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
          val visualization =
            UpsertVisualizationJob.updateVisualization(
              visualizationId,
              expressionId,
              module,
              config,
              callable,
              arguments
            )
          val stack = ctx.contextManager.getStack(config.executionContextId)
          val cachedValue = stack.headOption
            .flatMap(frame => Option(frame.cache.get(expressionId)))
          UpsertVisualizationJob.requireVisualizationSynchronization(
            stack,
            expressionId
          )
          cachedValue match {
            case Some(value) =>
              ProgramExecutionSupport.sendVisualizationUpdate(
                config.executionContextId,
                stack.headOption.get.syncState,
                visualization,
                expressionId,
                value
              )
              None
            case None =>
              Some(Executable(config.executionContextId, stack))
          }
      }
    } finally {
      ctx.locking.releaseContextLock(config.executionContextId)
      logger.log(
        Level.FINEST,
        s"Kept context lock [UpsertVisualizationJob] for ${System.currentTimeMillis() - lockTimestamp} milliseconds"
      )
    }
  }

  private def replyWithExpressionFailedError(
    message: String,
    executionResult: Option[Api.ExecutionResult.Diagnostic]
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.endpoint.sendToClient(
      Api.Response(
        requestId,
        Api.VisualizationExpressionFailed(message, executionResult)
      )
    )
  }

  private def replyWithError(error: Api.Error)(implicit
    ctx: RuntimeContext
  ): Unit = {
    ctx.endpoint.sendToClient(Api.Response(requestId, error))
  }
}

object UpsertVisualizationJob {

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

  /** Upsert the provided visualization.
    *
    * @param visualization the visualization to update
    * @param ctx the runtime context
    */
  def upsertVisualization(
    visualization: Visualization
  )(implicit ctx: RuntimeContext, logger: TruffleLogger): Unit = {
    val visualizationConfig = visualization.config
    val expressionId        = visualization.expressionId
    val visualizationId     = visualization.id
    val maybeCallable =
      evaluateVisualizationExpression(
        visualizationConfig.visualizationModule,
        visualizationConfig.expression
      )

    maybeCallable.foreach { result =>
      updateVisualization(
        visualizationId,
        expressionId,
        result.module,
        visualizationConfig,
        result.callback,
        result.arguments
      )
      val stack =
        ctx.contextManager.getStack(visualizationConfig.executionContextId)
      requireVisualizationSynchronization(stack, expressionId)
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

  /** Evaluate the visualization expression in a given module.
    *
    * @param module the module where to evaluate arguments for the expression
    * @param expression the visualization expression
    * @param expressionModule the module where to evaluate the expression
    * @param retryCount the number of attempted retries
    * @param ctx the runtime context
    * @return either the evaluation result or an evaluation failure
    */
  private def evaluateModuleExpression(
    module: Module,
    expression: Api.VisualizationExpression,
    expressionModule: Module,
    retryCount: Int = 0
  )(implicit
    ctx: RuntimeContext
  ): Either[EvaluationFailure, EvaluationResult] =
    Either
      .catchNonFatal {
        val (callback, arguments) = expression match {
          case Api.VisualizationExpression.Text(_, expression) =>
            val callback = ctx.executionService.evaluateExpression(
              expressionModule,
              expression
            )
            val arguments = Vector()
            (callback, arguments)
          case Api.VisualizationExpression.ModuleMethod(
                Api.MethodPointer(_, definedOnType, name),
                argumentExpressions
              ) =>
            val callback = ctx.executionService.prepareFunctionCall(
              expressionModule,
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
            s"Evaluation of visualization was interrupted. Retrying [${retryCount + 1}]."
          )
          evaluateModuleExpression(
            module,
            expression,
            expressionModule,
            retryCount + 1
          )

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

  /** Evaluate the visualization expression.
    *
    * @param module module to evaluate the expression arguments at
    * @param expression the visualization expression to evaluate
    * @param ctx the runtime context
    * @return either the evaluation result or an evaluation error
    */
  private def evaluateVisualizationExpression(
    module: String,
    expression: Api.VisualizationExpression
  )(implicit
    ctx: RuntimeContext
  ): Either[EvaluationFailure, EvaluationResult] = {
    for {
      module           <- findModule(module)
      expressionModule <- findModule(expression.module)
      expression <- evaluateModuleExpression(
        module,
        expression,
        expressionModule
      )
    } yield expression
  }

  /** Update the visualization state.
    *
    * @param visualizationId the visualization identifier
    * @param expressionId the expression to which the visualization is applied
    * @param module the module containing the visualization
    * @param visualizationConfig the visualization configuration
    * @param callback the visualization callback function
    * @param arguments the list of arugments that will be passed to the callback
    * @param ctx the runtime context
    * @return the re-evaluated visualization
    */
  private def updateVisualization(
    visualizationId: Api.VisualizationId,
    expressionId: Api.ExpressionId,
    module: Module,
    visualizationConfig: Api.VisualizationConfiguration,
    callback: AnyRef,
    arguments: Vector[AnyRef]
  )(implicit ctx: RuntimeContext, logger: TruffleLogger): Visualization = {
    val visualizationExpressionId =
      findVisualizationExpressionId(module, visualizationConfig.expression)
    val visualization = Visualization(
      visualizationId,
      expressionId,
      new RuntimeCache(),
      module,
      visualizationConfig,
      visualizationExpressionId,
      callback,
      arguments
    )
    val writeLockTimestamp = ctx.locking.acquireWriteCompilationLock()
    try {
      invalidateCaches(visualization)
    } finally {
      ctx.locking.releaseWriteCompilationLock()
      logger.log(
        Level.FINEST,
        s"Kept write compilation lock [UpsertVisualizationJob] for ${System.currentTimeMillis() - writeLockTimestamp} milliseconds"
      )
    }
    ctx.contextManager.upsertVisualization(
      visualizationConfig.executionContextId,
      visualization
    )
    visualization
  }

  /** Find the expressionId of visualization function.
    *
    * @param module the module environment
    * @param visualizationExpression the visualization expression
    * @return the expression id of required visualization function
    */
  private def findVisualizationExpressionId(
    module: Module,
    visualizationExpression: Api.VisualizationExpression
  ): Option[Api.ExpressionId] =
    visualizationExpression match {
      case Api.VisualizationExpression.ModuleMethod(methodPointer, _) =>
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

      case _: Api.VisualizationExpression.Text => None
    }

  /** Update the caches. */
  private def invalidateCaches(
    visualization: Visualization
  )(implicit ctx: RuntimeContext): Unit = {
    setCacheWeights(visualization)
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
    if (!stacks.exists(isExpressionCached(visualization.expressionId, _))) {
      invalidateFirstDependent(visualization.expressionId)
    }
  }

  /** Check if the expression is cached in the execution stack.
    *
    * @param expressionId the expression id to check
    * @param stack the execution stack
    * @return `true` if the expression exists in the frame cache
    */
  private def isExpressionCached(
    expressionId: Api.ExpressionId,
    stack: Iterable[InstrumentFrame]
  ): Boolean = {
    stack.headOption.exists { frame =>
      frame.cache.get(expressionId) ne null
    }
  }

  /** Set the cache weights for the provided visualization.
    *
    * @param visualization the visualization to update
    */
  private def setCacheWeights(visualization: Visualization): Unit = {
    visualization.module.getIr.getMetadata(CachePreferenceAnalysis).foreach {
      metadata =>
        CacheInvalidation.runVisualizations(
          Seq(visualization),
          CacheInvalidation.Command.SetMetadata(metadata)
        )
    }
  }

  /** Invalidate the first cached dependent node of the provided expression.
    *
    * @param expressionId the expression id
    */
  private def invalidateFirstDependent(
    expressionId: Api.ExpressionId
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

  /** Require to send the visualization update.
    *
    * @param stack the execution stack
    * @param expressionId the expression id to which the visualization is applied
    */
  private def requireVisualizationSynchronization(
    stack: Iterable[InstrumentFrame],
    expressionId: Api.ExpressionId
  ): Unit =
    stack.foreach(_.syncState.setVisualizationUnsync(expressionId))

}
