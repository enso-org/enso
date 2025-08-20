package org.enso.interpreter.instrument.job

import org.slf4j.LoggerFactory
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.Function
import org.enso.compiler.core.ir.Name
import org.enso.compiler.core.ir.module.scope.{definition, Definition}
import org.enso.compiler.refactoring.IRUtils
import org.enso.compiler.pass.analyse.{
  CachePreferenceAnalysis,
  DataflowAnalysis
}
import org.enso.interpreter.instrument.execution.{Executable, RuntimeContext}
import org.enso.interpreter.instrument.job.UpsertVisualizationJob.{
  EvaluationFailed,
  EvaluationResult,
  ModuleNotFound,
  RequiresCompilation
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

import java.util.UUID
import scala.annotation.unused
import scala.concurrent.ExecutionException
import scala.util.Try

/** A job that upserts a visualization.
  *
  * @param requestId maybe a request id
  * @param visualizationId an identifier of visualization
  * @param expressionId an identifier of expression
  * @param config a visualization config
  */
class UpsertVisualizationJob(
  @unused requestId: Option[Api.RequestId],
  val visualizationId: Api.VisualizationId,
  val expressionId: Api.ExpressionId,
  config: Api.VisualizationConfiguration
) extends Job[Option[Executable]](
      List(config.executionContextId),
      false,
      false,
      true
    )
    with UniqueJob[Option[Executable]] {

  /** @inheritdoc */
  override def equalsTo(that: UniqueJob[_]): Boolean =
    that match {
      case that: UpsertVisualizationJob =>
        this.expressionId == that.expressionId && this.visualizationId == that.visualizationId
      case _ => false
    }

  /** @inheritdoc */
  override def runImpl(implicit ctx: RuntimeContext): Option[Executable] =
    ctx.locking.withReadContextLock(
      ctx.locking.getOrCreateContextLock(config.executionContextId),
      classOf[UpsertVisualizationJob],
      () => {
        val (needsRetryWithWriteLock, maybeResult) =
          ctx.locking.withReadCompilationLock(
            classOf[UpsertVisualizationJob],
            () =>
              evaluateAndExecuteVisualization(
                hasWriteLock = false
              )
          )
        if (needsRetryWithWriteLock) {
          UpsertVisualizationJob.logger.trace(
            "Retrying visualization {} evaluation with write lock to compile necessary modules",
            visualizationId
          )
          ctx.locking.withWriteCompilationLock(
            classOf[UpsertVisualizationJob],
            "visualizationId=" + visualizationId + ",expressionId=" + expressionId,
            () =>
              evaluateAndExecuteVisualization(
                hasWriteLock = true
              )._2
          )
        } else {
          maybeResult
        }
      }
    )

  /** Attempts to evaluate the visualization expression associated with this job.
    *
    * @param value computed value to be visualized
    * @param runtimeCache an instance of runtime cache associated with this frame
    * @param stack current stackframe
    * @param hasWriteLock true if necessary module loading/compilation can be performed, if needed. False otherwise
    * @param ctx an instance of current `RuntimeContext`
    * @return true if failed due to required compilation and lack of required lock, false if successful
    */
  private def evaluateAndExecuteVisualization(
    hasWriteLock: Boolean
  )(implicit ctx: RuntimeContext): (Boolean, Option[Executable]) = {
    UpsertVisualizationJob.logger.trace(
      "Evaluating expression {} in observer",
      expressionId
    )
    val maybeCallable = UpsertVisualizationJob.evaluateVisualizationExpression(
      config.visualizationModule,
      config.expression,
      hasWriteLock
    )

    maybeCallable match {
      case Left(ModuleNotFound(moduleName)) =>
        UpsertVisualizationJob.logger.trace(
          "Evaluation of visualization {} in observer for expression {} failed. Module not found",
          visualizationId,
          expressionId
        )
        ctx.endpoint.sendToClient(
          Api.Response(Api.ModuleNotFound(moduleName))
        )
        (false, None)
      case Left(EvaluationFailed(message, result)) =>
        UpsertVisualizationJob.logger.trace(
          "Evaluation of visualization {} in observer for expression {} failed.",
          visualizationId,
          expressionId
        )
        replyWithExpressionFailedError(
          config.executionContextId,
          visualizationId,
          expressionId,
          message,
          result
        )
        (false, None)
      case Left(RequiresCompilation) =>
        // todo reply with expr failed
        (!hasWriteLock, None)
      case Right(evaluatedExpression) =>
        (false, executeVisualization(evaluatedExpression))
    }
  }

  private def executeVisualization(
    evaluatedVisualization: EvaluationResult
  )(implicit ctx: RuntimeContext): Option[Executable] = {
    val EvaluationResult(module, callable, arguments) = evaluatedVisualization
    UpsertVisualizationJob.logger.trace(
      "Executing visualization {} for expression {}",
      visualizationId,
      expressionId
    )

    val visualization =
      UpsertVisualizationJob.updateAttachedVisualization(
        visualizationId,
        expressionId,
        module,
        config,
        callable,
        arguments
      )
    val stack =
      ctx.contextManager.getStack(config.executionContextId)
    val runtimeCache = stack.headOption
      .flatMap(frame => Option(frame.cache))
    val cachedValue = runtimeCache
      .flatMap(c => Option(c.get(expressionId)))
    UpsertVisualizationJob.requireVisualizationSynchronization(
      stack,
      visualizationId
    )
    cachedValue match {
      case Some(value) =>
        ProgramExecutionSupport.executeAndSendVisualizationUpdate(
          config.executionContextId,
          runtimeCache.getOrElse(new RuntimeCache),
          stack.headOption.get.syncState,
          visualization,
          expressionId,
          value
        )
        None
      case None =>
        UpsertVisualizationJob.logger.trace(
          "Cached value for expresion {}: missing",
          expressionId
        )
        Some(Executable(config.executionContextId, stack))
    }
  }

  private def replyWithExpressionFailedError(
    contextId: Api.ContextId,
    visualizationId: Api.VisualizationId,
    expressionId: Api.ExpressionId,
    message: String,
    executionResult: Option[Api.ExecutionResult.Diagnostic]
  )(implicit ctx: RuntimeContext): Unit = {
    UpsertVisualizationJob.logger.error(
      "Visualization for expression {} failed: {} (evaluation result: {})",
      expressionId,
      message,
      executionResult
    )
    ctx.endpoint.sendToClient(
      Api.Response(
        Api.VisualizationExpressionFailed(
          Api.VisualizationContext(visualizationId, contextId, expressionId),
          message,
          executionResult
        )
      )
    )
  }

  override def toString: String = {
    s"UpsertVisualizationJob(visualizationId=$visualizationId, expressionId=$expressionId)"
  }

}

object UpsertVisualizationJob {
  private lazy val logger =
    LoggerFactory.getLogger(classOf[UpsertVisualizationJob])

  /** Invalidate caches for a particular expression id. */
  sealed private case class InvalidateCaches(
    expressionId: Api.ExpressionId
  )(implicit ctx: RuntimeContext)
      extends Runnable {

    override def run(): Unit = {
      ctx.locking.withWriteCompilationLock(
        classOf[UpsertVisualizationJob],
        () => invalidateCaches(expressionId)
      )
    }
  }

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

  /** Signals that in order to evaluate a visualization expression,
    * involved modules need to be loaded/compiled first and necessary write compilation lock was missing
    */
  case object RequiresCompilation extends EvaluationFailure

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
  )(implicit ctx: RuntimeContext): Unit = {
    val visualizationConfig = visualization.config
    val expressionId        = visualization.expressionId
    val visualizationId     = visualization.id
    val maybeCallable =
      evaluateVisualizationExpression(
        visualizationConfig.visualizationModule,
        visualizationConfig.expression,
        hasWriteCompilationLock = true
      )

    maybeCallable.foreach { result =>
      updateAttachedVisualization(
        visualizationId,
        expressionId,
        result.module,
        visualizationConfig,
        result.callback,
        result.arguments
      )
      val stack =
        ctx.contextManager.getStack(visualizationConfig.executionContextId)
      requireVisualizationSynchronization(stack, visualizationId)
    }
  }

  /** Find module by name.
    *
    * @param moduleName the module name
    * @param hasWriteCompilationLock true if write compilation lock has been acquired, false otherwise
    * @return either the requested module or an error
    */
  private def findModule(
    moduleName: String,
    hasWriteCompilationLock: Boolean
  )(implicit ctx: RuntimeContext): Either[EvaluationFailure, Module] = {
    val context = ctx.executionService.getContext
    if (!context.moduleIsLoaded(moduleName)) {
      if (hasWriteCompilationLock) {
        context.ensureModuleIsLoaded(moduleName)
      } else {
        return Left(RequiresCompilation)
      }
    }
    val maybeModule = context.findModule(moduleName)
    if (maybeModule.isPresent) {
      val module = maybeModule.get()
      if (module.needsCompilation()) {
        if (hasWriteCompilationLock) {
          module.compileScope(context)
          Right(module)
        } else {
          Left(RequiresCompilation)
        }
      } else {
        Right(maybeModule.get())
      }
    } else Left(ModuleNotFound(moduleName))
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
    Try {
      val pending =
        ctx.executionService.evaluateExpression(
          module,
          argumentExpression,
          "evaluate args"
        )
      pending.toCompletableFuture.get()
    }.toEither.left.flatMap {
      case _: ThreadInterruptedException
          if retryCount < MaxEvaluationRetryCount =>
        evaluateArgumentExpression(
          module,
          argumentExpression,
          retryCount + 1
        )

      case error: ThreadInterruptedException =>
        UpsertVisualizationJob.logger.error(
          "Evaluation of visualization argument [{}] in module [{}] was interrupted [{}] times.",
          argumentExpression,
          module.getName.toString,
          retryCount: Integer,
          error
        )
        Left(
          EvaluationFailed(
            s"Evaluation of visualization argument was interrupted [$retryCount] times.",
            ProgramExecutionSupport.getDiagnosticOutcome(error)
          )
        )

      case error =>
        UpsertVisualizationJob.logger.error(
          "Evaluation of visualization argument [{}] failed in module [{}] with [{}]: {}",
          argumentExpression,
          module.getName.toString,
          error.getClass.getSimpleName,
          error.getMessage,
          error
        )
        Left(
          EvaluationFailed(
            Option(error.getMessage).getOrElse(error.getClass.getSimpleName),
            ProgramExecutionSupport.getDiagnosticOutcome(error)
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
    Try {
      val pending = expression match {
        case Api.VisualizationExpression.Text(_, expression, _) =>
          ctx.executionService.evaluateExpression(
            expressionModule,
            expression,
            "evaluate visualization function"
          )
        case Api.VisualizationExpression.ModuleMethod(
              Api.MethodPointer(_, definedOnType, name),
              _
            ) =>
          ctx.executionService
            .prepareFunctionCall(
              expressionModule,
              QualifiedName.fromString(definedOnType).item,
              name
            )
            .thenApply(f => f.asInstanceOf[AnyRef])
      }
      pending.toCompletableFuture.get()
    }.toEither.left.flatMap {
      case _: ThreadInterruptedException
          if retryCount < MaxEvaluationRetryCount =>
        evaluateVisualizationFunction(
          expression,
          expressionModule,
          retryCount + 1
        )

      case error: ThreadInterruptedException =>
        UpsertVisualizationJob.logger.error(
          "Evaluation of visualization [{}] in module [{}] was interrupted [{}] times.",
          expression,
          expressionModule,
          retryCount: Integer,
          error
        )
        Left(
          EvaluationFailed(
            s"Evaluation of visualization was interrupted [$retryCount] times.",
            ProgramExecutionSupport.getDiagnosticOutcome(error)
          )
        )
      case execError: ExecutionException if execError.getCause != null =>
        val error = execError.getCause
        UpsertVisualizationJob.logger.error(
          "Evaluation of visualization [{}] failed in module [{}] with [{}]: {}",
          expression,
          expressionModule,
          error.getClass,
          error.getMessage,
          error
        )
        Left(
          EvaluationFailed(
            Option(error.getMessage).getOrElse(error.getClass.getSimpleName),
            ProgramExecutionSupport.getDiagnosticOutcome(error)
          )
        )
      case error =>
        UpsertVisualizationJob.logger.error(
          "Evaluation of visualization [{}] failed in module [{}] with [{}]: {}",
          expression,
          expressionModule,
          error.getClass,
          error.getMessage,
          error
        )
        Left(
          EvaluationFailed(
            Option(error.getMessage).getOrElse(error.getClass.getSimpleName),
            ProgramExecutionSupport.getDiagnosticOutcome(error)
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
    * @param hasWriteCompilationLock true if write compilation lock has been acquired, false otherwise
    * @param ctx the runtime context
    * @return either the evaluation result or an evaluation error
    */
  private def evaluateVisualizationExpression(
    module: String,
    expression: Api.VisualizationExpression,
    hasWriteCompilationLock: Boolean
  )(implicit
    ctx: RuntimeContext
  ): Either[EvaluationFailure, EvaluationResult] = {
    for {
      module           <- findModule(module, hasWriteCompilationLock)
      expressionModule <- findModule(expression.module, hasWriteCompilationLock)
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
  def updateAttachedVisualization(
    visualizationId: Api.VisualizationId,
    expressionId: Api.ExpressionId,
    module: Module,
    visualizationConfig: Api.VisualizationConfiguration,
    callback: AnyRef,
    arguments: Vector[AnyRef]
  )(implicit ctx: RuntimeContext): Visualization = {
    val visualizationExpressionId =
      findVisualizationExpressionId(module, visualizationConfig.expression)
    val visualization =
      Visualization(
        visualizationId,
        expressionId,
        new RuntimeCache(),
        module,
        visualizationConfig,
        visualizationExpressionId,
        callback,
        arguments
      )
    setCacheWeights(visualization)
    ctx.state.executionHooks.add(InvalidateCaches(expressionId))
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
          .collectFirst {
            case ExternalIdOfMethod(externalId, methodReference)
                if methodReference.methodName.name == methodPointer.name =>
              externalId
          }
      case _: Api.VisualizationExpression.Text => None
    }

  private object ExternalIdOfMethod {
    def unapply(d: Definition): Option[(UUID, Name.MethodReference)] = {
      d match {
        case method: definition.Method =>
          val methodReference        = method.methodReference
          val methodReferenceTypeOpt = methodReference.typePointer.map(_.name)

          Option
            .when(methodReferenceTypeOpt.isEmpty)(
              method.body match {
                case fun: Function => fun.body.getExternalId
                case _             => method.getExternalId
              }
            )
            .flatten
            .map((_, methodReference))
        case _ =>
          None
      }
    }
  }

  /** Update the caches. */
  private def invalidateCaches(
    expressionId: Api.ExpressionId
  )(implicit ctx: RuntimeContext): Unit = {
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
    if (!stacks.exists(isExpressionCached(expressionId, _))) {
      invalidateFirstDependent(expressionId)
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
    visualization.module.getIr
      .getMetadata(CachePreferenceAnalysis)
      .foreach { metadata =>
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
            IRUtils
              .findByExternalId(module.getIr, externalId)
              .map { ir =>
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
                              .InvalidateKeys(
                                Seq(firstDependent),
                                "first dependendent of " + expressionId + " in upsert"
                              )
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
    * @param visualizationId the visualization id associated with the expression
    */
  private def requireVisualizationSynchronization(
    stack: Iterable[InstrumentFrame],
    visualizationId: Api.VisualizationId
  ): Unit =
    stack.foreach(_.syncState.setVisualizationUnsync(visualizationId))

}
