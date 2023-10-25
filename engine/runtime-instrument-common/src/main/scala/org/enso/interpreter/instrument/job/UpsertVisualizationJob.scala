package org.enso.interpreter.instrument.job

import cats.implicits._
import com.oracle.truffle.api.TruffleLogger
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.Function
import org.enso.compiler.core.ir.Name
import org.enso.compiler.core.ir.module.scope.definition
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
  val visualizationId: Api.VisualizationId,
  val expressionId: Api.ExpressionId,
  config: Api.VisualizationConfiguration
) extends Job[Option[Executable]](
      List(config.executionContextId),
      false,
      false
    )
    with UniqueJob[Option[Executable]] {

  /** @inheritdoc */
  override def equalsTo(that: UniqueJob[_]): Boolean =
    that match {
      case that: UpsertVisualizationJob =>
        this.expressionId == that.expressionId
      case _ => false
    }

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Option[Executable] = {
    implicit val logger: TruffleLogger = ctx.executionService.getLogger
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
        "Kept context lock [{0}] for {1} milliseconds.",
        Array(
          getClass.getSimpleName,
          System.currentTimeMillis() - lockTimestamp
        )
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
  private val MaxEvaluationRetryCount: Int = 5

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

  /** Evaluate the visualization arguments in a given module.
    *
    * @param module the module where to evaluate arguments for the expression
    * @param argumentExpressions the list of argument expression to the visualization function
    * @param ctx the runtime context
    * @return either the evaluation result or an evaluation failure
    */
  private def evaluateArgumentExpressions(
    module: Module,
    argumentExpressions: Vector[String],
    retryCount: Int = 0
  )(implicit
    ctx: RuntimeContext
  ): Either[EvaluationFailure, Vector[AnyRef]] = {
    val z: Either[EvaluationFailure, Vector[AnyRef]] = Right(Vector())
    argumentExpressions.foldLeft(z) { (result, expr) =>
      for {
        acc <- result
        res <- evaluateArgumentExpression(module, expr, retryCount)
      } yield acc :+ res
    }
  }

  /** Evaluate the visualization argument in a given module.
    *
    * @param module the module where to evaluate arguments for the expression
    * @param argumentExpression the argument expression to the visualization function
    * @param ctx the runtime context
    * @return either the evaluation result or an evaluation failure
    */
  private def evaluateArgumentExpression(
    module: Module,
    argumentExpression: String,
    retryCount: Int
  )(implicit
    ctx: RuntimeContext
  ): Either[EvaluationFailure, AnyRef] = {
    Either
      .catchNonFatal {
        ctx.executionService.evaluateExpression(module, argumentExpression)
      }
      .leftFlatMap {
        case _: ThreadInterruptedException
            if retryCount < MaxEvaluationRetryCount =>
          evaluateArgumentExpression(
            module,
            argumentExpression,
            retryCount + 1
          )

        case error: ThreadInterruptedException =>
          ctx.executionService.getLogger.log(
            Level.SEVERE,
            "Evaluation of visualization argument [{0}] in module [{1}] was interrupted [{2}] times.",
            Array[Object](
              argumentExpression,
              module.getName.toString,
              retryCount: Integer,
              error
            )
          )
          Left(
            EvaluationFailed(
              s"Evaluation of visualization argument was interrupted [$retryCount] times.",
              ProgramExecutionSupport.getDiagnosticOutcome.lift(error)
            )
          )

        case error =>
          ctx.executionService.getLogger.log(
            Level.SEVERE,
            "Evaluation of visualization argument [{0}] failed in module [{1}] with [{2}]: {3}",
            Array[Object](
              argumentExpression,
              module.getName.toString,
              error.getClass.getSimpleName,
              error.getMessage,
              error
            )
          )
          Left(
            EvaluationFailed(
              Option(error.getMessage).getOrElse(error.getClass.getSimpleName),
              ProgramExecutionSupport.getDiagnosticOutcome.lift(error)
            )
          )

      }
  }

  /** Evaluate the visualization expression in a given module.
    *
    * @param expression the visualization expression
    * @param expressionModule the module where to evaluate the expression
    * @param retryCount the number of attempted retries
    * @param ctx the runtime context
    * @return either the evaluation result or an evaluation failure
    */
  private def evaluateVisualizationFunction(
    expression: Api.VisualizationExpression,
    expressionModule: Module,
    retryCount: Int
  )(implicit
    ctx: RuntimeContext
  ): Either[EvaluationFailure, AnyRef] =
    Either
      .catchNonFatal {
        expression match {
          case Api.VisualizationExpression.Text(_, expression) =>
            ctx.executionService.evaluateExpression(
              expressionModule,
              expression
            )
          case Api.VisualizationExpression.ModuleMethod(
                Api.MethodPointer(_, definedOnType, name),
                _
              ) =>
            ctx.executionService.prepareFunctionCall(
              expressionModule,
              QualifiedName.fromString(definedOnType).item,
              name
            )
        }
      }
      .leftFlatMap {
        case _: ThreadInterruptedException
            if retryCount < MaxEvaluationRetryCount =>
          evaluateVisualizationFunction(
            expression,
            expressionModule,
            retryCount + 1
          )

        case error: ThreadInterruptedException =>
          ctx.executionService.getLogger.log(
            Level.SEVERE,
            "Evaluation of visualization [{0}] in module [{1}] was interrupted [{2}] times.",
            Array[Object](
              expression,
              expressionModule,
              retryCount: Integer,
              error
            )
          )
          Left(
            EvaluationFailed(
              s"Evaluation of visualization was interrupted [$retryCount] times.",
              ProgramExecutionSupport.getDiagnosticOutcome.lift(error)
            )
          )

        case error =>
          ctx.executionService.getLogger.log(
            Level.SEVERE,
            "Evaluation of visualization [{0}] failed in module [{1}] with [{2}]: {3}",
            Array[Object](
              expression,
              expressionModule,
              error.getClass,
              error.getMessage,
              error
            )
          )
          Left(
            EvaluationFailed(
              Option(error.getMessage).getOrElse(error.getClass.getSimpleName),
              ProgramExecutionSupport.getDiagnosticOutcome.lift(error)
            )
          )
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
  ): Either[EvaluationFailure, EvaluationResult] = {
    for {
      callback <- evaluateVisualizationFunction(
        expression,
        expressionModule,
        retryCount
      )
      arguments <- evaluateArgumentExpressions(
        module,
        expression.positionalArgumentsExpressions
      )
    } yield EvaluationResult(module, callback, arguments)
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
      evaluationResult <- evaluateModuleExpression(
        module,
        expression,
        expressionModule
      )
    } yield evaluationResult
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
          .collect { case method: definition.Method =>
            val methodReference        = method.methodReference
            val methodReferenceName    = methodReference.methodName.name
            val methodReferenceTypeOpt = methodReference.typePointer.map(_.name)

            val externalIdOpt = method.body match {
              case fun: Function => fun.body.getExternalId
              case _             => method.getExternalId
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
            val externalId = expressionId
            module.getIr.preorder
              .find(_.getExternalId.contains(externalId))
              .collect {
                case name: Name.Literal =>
                  DataflowAnalysis.DependencyInfo.Type
                    .Dynamic(name.name, Some(externalId))
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
