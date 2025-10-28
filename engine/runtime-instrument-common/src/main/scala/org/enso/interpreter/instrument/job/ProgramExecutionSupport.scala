package org.enso.interpreter.instrument.job

import org.slf4j.LoggerFactory
import com.oracle.truffle.api.exception.AbstractTruffleException
import com.oracle.truffle.api.source.SourceSection
import org.enso.interpreter.instrument.{
  InstrumentFrame,
  MethodCallsCache,
  RuntimeCache,
  TypeInfo,
  UpdatesSynchronizationState,
  Visualization,
  WarningPreview
}
import org.enso.interpreter.instrument.execution.{ErrorResolver, RuntimeContext}
import org.enso.interpreter.instrument.profiling.ExecutionTime
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode.FunctionCall
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode
import org.enso.interpreter.runtime.`type`.{Types, TypesGen}
import org.enso.interpreter.runtime.data.atom.AtomConstructor
import org.enso.interpreter.runtime.callable.function.Function
import org.enso.interpreter.runtime.control.ThreadInterruptedException
import org.enso.interpreter.runtime.error.{DataflowError, PanicSentinel}
import org.enso.interpreter.service.ExecutionService.{
  ExpressionCall,
  ExpressionValue,
  FunctionPointer
}
import org.enso.interpreter.service.error.{
  MethodNotFoundException,
  ModuleNotFoundForExpressionIdException,
  ServiceException,
  TypeNotFoundException,
  VisualizationException
}
import org.enso.interpreter.runtime.warning.{
  Warning,
  WarningsLibrary,
  WithWarnings
}
import org.enso.polyglot.debugger.ExecutedVisualization
import org.enso.polyglot.runtime.Runtime.Api

import java.io.File
import java.util.UUID
import java.util.concurrent.{CompletionStage, ExecutionException}
import java.util.function.{Consumer, Supplier}
import scala.jdk.OptionConverters.RichOptional
import scala.util.Try

/** Provides support for executing Enso code. Adds convenient methods to
  * run Enso programs in a Truffle context.
  */
object ProgramExecutionSupport {
  private lazy val logger = LoggerFactory.getLogger(this.getClass)

  /** Runs the program.
    *
    * @param contextId an identifier of an execution context
    * @param executionFrame an execution frame
    * @param callStack a call stack
    */
  @scala.annotation.tailrec
  final private def executeProgram(
    contextId: Api.ContextId,
    executionFrame: ExecutionFrame,
    callStack: List[LocalCallFrame]
  )(implicit ctx: RuntimeContext): Unit = {

    val methodCallsCache = new MethodCallsCache
    var enterables       = Map[UUID, FunctionCall]()

    val onCachedMethodCallCallback: Consumer[ExpressionValue] = { value =>
      logger.trace("ON_CACHED_CALL {}", value.getExpressionId)
      sendExpressionUpdate(contextId, executionFrame.syncState, value)
    }

    val onCachedValueCallback: Consumer[ExpressionValue] = { value =>
      if (callStack.isEmpty) {
        logger.trace("ON_CACHED_VALUE {}", value.getExpressionId)
        sendExpressionUpdate(contextId, executionFrame.syncState, value)
        sendVisualizationUpdates(
          contextId,
          executionFrame.cache,
          executionFrame.syncState,
          value
        )
      }
    }

    val onComputedValueCallback: Consumer[ExpressionValue] = { value =>
      if (callStack.isEmpty) {
        logger.trace("ON_COMPUTED {}", value.getExpressionId)

        value.getValue match {
          case sentinel: PanicSentinel =>
            if (VisualizationResult.isInterruptedException(sentinel.getPanic)) {
              sendInterruptedExpressionUpdate(
                contextId,
                executionFrame.syncState,
                value
              )
              // Bail out early. Any references to this value that do not expect
              // Interrupted error will likely return `No_Such_Method` otherwise.
              throw new ThreadInterruptedException(sentinel.getPanic)
            }
          case _ =>
        }
        sendExpressionUpdate(contextId, executionFrame.syncState, value)
        sendVisualizationUpdates(
          contextId,
          executionFrame.cache,
          executionFrame.syncState,
          value
        )
      }
    }

    val callablesCallback: Consumer[ExpressionCall] = fun =>
      if (callStack.headOption.exists(_.expressionId == fun.getExpressionId)) {
        enterables += fun.getExpressionId -> fun.getCall
      }

    val pendingResult = executionFrame match {
      case ExecutionFrame(
            ExecutionItem.Method(module, cons, function),
            cache,
            syncState
          ) =>
        val onExecutedVisualizationCallback: Consumer[ExecutedVisualization] = {
          executedVisualization =>
            val visualizationResult =
              Either.cond(
                executedVisualization.error() eq null,
                executedVisualization.result(),
                executedVisualization.error()
              )
            sendVisualizationUpdate(
              visualizationResult,
              contextId,
              syncState,
              executedVisualization.visualizationId(),
              executedVisualization.expressionId(),
              executedVisualization.expressionValue()
            )
        }

        ctx.executionService.execute(
          module.toString,
          cons.item,
          function,
          ctx.contextManager.getVisualizationHolder(contextId),
          cache,
          methodCallsCache,
          syncState,
          callStack.headOption.map(_.expressionId).orNull,
          ctx.state.expressionExecutionState,
          callablesCallback,
          onComputedValueCallback,
          onCachedValueCallback,
          onExecutedVisualizationCallback
        )
      case ExecutionFrame(
            ExecutionItem.CallData(expressionId, callData),
            cache,
            syncState
          ) =>
        val onExecutedVisualizationCallback: Consumer[ExecutedVisualization] = {
          executedVisualization =>
            val visualizationResult =
              Either.cond(
                executedVisualization.error() eq null,
                executedVisualization.result(),
                executedVisualization.error()
              )
            sendVisualizationUpdate(
              visualizationResult,
              contextId,
              syncState,
              executedVisualization.visualizationId(),
              executedVisualization.expressionId(),
              executedVisualization.expressionValue()
            )
        }
        val module =
          ctx.executionService.getContext
            .findModuleByExpressionId(expressionId)
            .orElseThrow(() =>
              new ModuleNotFoundForExpressionIdException(expressionId)
            )
        ctx.executionService.execute(
          ctx.contextManager.getVisualizationHolder(contextId),
          module,
          callData,
          cache,
          methodCallsCache,
          syncState,
          callStack.headOption.map(_.expressionId).orNull,
          ctx.state.expressionExecutionState,
          callablesCallback,
          onComputedValueCallback,
          onCachedValueCallback,
          onExecutedVisualizationCallback
        )
    }

    // ensure it is finished
    pendingResult.toCompletableFuture.get()
    callStack match {
      case Nil =>
        val notExecuted =
          methodCallsCache.getNotExecuted(executionFrame.cache.getCalls)
        notExecuted.forEach { expressionId =>
          val expressionTypes = executionFrame.cache.getType(expressionId)
          val expressionCall  = executionFrame.cache.getCall(expressionId)
          onCachedMethodCallCallback.accept(
            new ExpressionValue(
              expressionId,
              null,
              expressionTypes,
              expressionTypes,
              expressionCall,
              expressionCall,
              Array(ExecutionTime.empty()),
              true,
              -1.0,
              null
            )
          )
        }
      case item :: tail =>
        enterables.get(item.expressionId) match {
          case Some(call) =>
            val executionFrame =
              ExecutionFrame(
                ExecutionItem.CallData(item.expressionId, call),
                item.cache,
                item.syncState
              )
            executeProgram(contextId, executionFrame, tail)
          case None =>
            ()
        }
    }
  }

  /** Runs the program.
    *
    * @param contextId an identifier of an execution context
    * @param stack a call stack
    * @param ctx a runtime context
    * @return an execution result
    */
  final def runProgram(
    contextId: Api.ContextId,
    stack: List[InstrumentFrame]
  )(implicit ctx: RuntimeContext): Option[Api.ExecutionResult] = {
    logger.trace(s"Run program {}", contextId)
    @scala.annotation.tailrec
    def unwind(
      stack: List[InstrumentFrame],
      explicitCalls: List[ExecutionFrame],
      localCalls: List[LocalCallFrame]
    ): (Option[ExecutionFrame], List[LocalCallFrame]) =
      stack match {
        case Nil =>
          (explicitCalls.lastOption, localCalls)
        case List(
              InstrumentFrame(call: Api.StackItem.ExplicitCall, cache, sync)
            ) =>
          (
            Some(ExecutionFrame(ExecutionItem.Method(call), cache, sync)),
            localCalls
          )
        case InstrumentFrame(Api.StackItem.LocalCall(id), cache, sync) :: xs =>
          unwind(
            xs,
            explicitCalls,
            LocalCallFrame(id, cache, sync) :: localCalls
          )
        case _ => throw new MatchError(stack)
      }

    val (explicitCallOpt, localCalls) = unwind(stack, Nil, Nil)
    val executionResult: Either[Option[Api.ExecutionResult], Unit] = for {
      stackItem <-
        explicitCallOpt.toRight(
          Some(Api.ExecutionResult.Failure("Execution stack is empty.", None))
        )
      _ <-
        Try(
          executeProgram(contextId, stackItem, localCalls)
        ).toEither.left
          .map(onExecutionError(stackItem.item, _))
    } yield ()
    logger.trace("Execution finished: {}", executionResult)
    executionResult.fold(identity, _ => None)
  }

  /** Execution error handler.
    *
    * @param item the stack item being executed
    * @param error the execution error
    * @return the error message
    */
  private def onExecutionError(
    item: ExecutionItem,
    t: Throwable
  )(implicit ctx: RuntimeContext): Option[Api.ExecutionResult] = {
    val itemName = item match {
      case ExecutionItem.Method(_, _, function) => function
      case ExecutionItem.CallData(_, call)      => call.getFunction.getName
    }
    val error = t match {
      case e: ExecutionException if e.getCause != null => e.getCause
      case e                                           => e
    }
    val executionUpdate = getExecutionOutcome(error)
    val reason          = VisualizationResult.findExceptionMessage(error)
    def onFailure(): Option[Api.ExecutionResult] = error match {
      case _: ThreadInterruptedException =>
        logger.trace("Execution of function {} interrupted.", itemName)
        None
      case _ =>
        val message = error.getClass.getSimpleName
        logger.trace(
          "Execution of function {} failed ({}).",
          itemName,
          reason,
          error
        )
        Some(Api.ExecutionResult.Failure(message, None))
    }
    executionUpdate.orElse(onFailure())
  }

  /** Convert the runtime exception to the corresponding API error messages.
    *
    * @param t the exception
    * @param ctx the runtime context
    * @return the API message describing the error
    */
  private def getExecutionOutcome(
    t: Throwable
  )(implicit ctx: RuntimeContext): Option[Api.ExecutionResult] = {
    val pendingDiagnostic =
      ctx.executionService.getDiagnosticOutcome(t)
    pendingDiagnostic
      .thenApply(
        _.map(d => Option(d.asInstanceOf[Api.ExecutionResult]))
          .orElse(getFailureOutcomeFromException(t))
      )
      .toCompletableFuture
      .get()
  }

  def getDiagnosticOutcome(
    t: Throwable
  )(implicit ctx: RuntimeContext): Option[Api.ExecutionResult.Diagnostic] = {
    val pendingDiagnostic = ctx.executionService.getDiagnosticOutcome(t)
    pendingDiagnostic
      .thenApply(diagnostic => {
        // Can't use `orElse` since type inferencer gets confused
        if (diagnostic.isEmpty) None
        else Some(diagnostic.get().asInstanceOf[Api.ExecutionResult.Diagnostic])
      })
      .toCompletableFuture
      .get()
  }

  private def getFailureOutcomeFromException(throwable: Throwable)(implicit
    ctx: RuntimeContext
  ): Option[Api.ExecutionResult] = throwable match {
    case ex: TypeNotFoundException =>
      Some(
        Api.ExecutionResult.Failure(
          ex.getMessage,
          findFileByModuleName(ex.getModule)
        )
      )
    case ex: MethodNotFoundException =>
      Some(
        Api.ExecutionResult.Failure(
          ex.getMessage,
          findFileByModuleName(ex.getModule)
        )
      )
    case ex: ServiceException =>
      Some(Api.ExecutionResult.Failure(ex.getMessage, None))
    case _ =>
      None
  }

  private def sendInterruptedExpressionUpdate(
    contextId: Api.ContextId,
    syncState: UpdatesSynchronizationState,
    value: ExpressionValue
  )(implicit ctx: RuntimeContext): Unit = {
    val expressionId = value.getExpressionId
    val methodCall   = toMethodCall(value)
    if (
      !syncState.isExpressionSync(expressionId) ||
      (methodCall.isDefined && !syncState.isMethodPointerSync(
        expressionId
      ))
    ) {
      val payload =
        Api.ExpressionUpdate.Payload.Pending(
          None,
          None,
          wasInterrupted = true
        )
      ctx.endpoint.sendToClient(
        Api.Response(
          Api.ExpressionUpdates(
            contextId,
            Set(
              Api.ExpressionUpdate(
                value.getExpressionId,
                Option(value.getType).map(toExpressionType),
                methodCall,
                value.getProfilingInfo.map { case e: ExecutionTime =>
                  Api.ProfilingInfo.ExecutionTime(e.getNanoTimeElapsed)
                }.toVector,
                value.wasCached(),
                value.isTypeChanged || value.isFunctionCallChanged,
                payload
              )
            )
          )
        )
      )

      syncState.setExpressionSync(expressionId)
      ctx.state.expressionExecutionState.setExpressionExecuted(expressionId)
      if (methodCall.isDefined) {
        syncState.setMethodPointerSync(expressionId)
      }
    }
  }

  private def sendExpressionUpdate(
    contextId: Api.ContextId,
    syncState: UpdatesSynchronizationState,
    value: ExpressionValue
  )(implicit ctx: RuntimeContext): Unit = {
    val expressionId = value.getExpressionId
    if (value.isProgressUpdate()) {
      val progressPayload = Api.ExpressionUpdate.Payload.Pending(
        Option(value.getProgressMessage()),
        Some(value.getProgress())
      )
      ctx.endpoint.sendToClient(
        Api.Response(
          Api.ExpressionUpdates(
            contextId,
            Set(
              Api.ExpressionUpdate(
                value.getExpressionId,
                None,
                None,
                Vector(),
                false,
                false,
                progressPayload
              )
            )
          )
        )
      )
      return
    }
    val methodCall = toMethodCall(value)
    if (
      !syncState.isExpressionSync(expressionId) ||
      (
        methodCall.isDefined && !syncState.isMethodPointerSync(
          expressionId
        )
      ) ||
      Types.isPanic(value.getType.visibleType())
    ) {
      val payload = value.getValue match {
        case sentinel: PanicSentinel =>
          val exceptionMsg =
            ctx.executionService
              .getExceptionMessage(sentinel.getPanic)
              .toCompletableFuture
              .get()
          Some(
            Api.ExpressionUpdate.Payload
              .Panic(
                exceptionMsg,
                ErrorResolver
                  .getStackTrace(sentinel)(ctx.executionService)
                  .flatMap(_.expressionId)
              )
          )
        case error: DataflowError =>
          Some(
            Api.ExpressionUpdate.Payload.DataflowError(
              ErrorResolver
                .getStackTrace(error)(ctx.executionService)
                .flatMap(_.expressionId)
            )
          )
        case panic: AbstractTruffleException =>
          if (!VisualizationResult.isInterruptedException(panic)) {
            Some(
              Api.ExpressionUpdate.Payload.Panic(
                VisualizationResult.findExceptionMessage(panic),
                ErrorResolver
                  .getStackTrace(panic)(ctx.executionService)
                  .flatMap(_.expressionId)
              )
            )
          } else {
            logger.trace("Computation of expression has been interrupted")
            None
          }
        case warnings: WithWarnings
            if warnings.getValue.isInstanceOf[DataflowError] =>
          Some(
            Api.ExpressionUpdate.Payload.DataflowError(
              ErrorResolver
                .getStackTrace(warnings.getValue.asInstanceOf[DataflowError])(
                  ctx.executionService
                )
                .flatMap(_.expressionId)
            )
          )
        case _ =>
          val warnings =
            Option.when(
              value.getValue != null && WarningsLibrary.getUncached.hasWarnings(
                value.getValue
              )
            ) {
              val warnsMap =
                WarningsLibrary.getUncached.getWarnings(value.getValue, false)
              val warnings      = Warning.fromMapToArray(warnsMap)
              val warningsCount = warnings.length
              val warning =
                if (warningsCount > 0) {
                  Try(
                    WarningPreview
                      .execute(warnings(0).getValue)
                      .toCompletableFuture
                      .get()
                  ).toEither
                    .fold(
                      error => {
                        logger.trace(
                          "Failed to execute warning preview of expression [{}].",
                          expressionId,
                          error
                        )
                        None
                      },
                      Some(_)
                    )
                } else {
                  None
                }

              Api.ExpressionUpdate.Payload.Value
                .Warnings(
                  warningsCount,
                  warning,
                  WarningsLibrary.getUncached.isLimitReached(value.getValue)
                )
            }

          val schema = value.getValue match {
            case function: Function =>
              val functionInfo = FunctionPointer.fromFunction(function)
              val notAppliedArguments = FunctionPointer
                .collectNotAppliedArguments(function)
                .toVector
              toMethodPointer(functionInfo).map(methodPointer =>
                Api.FunctionSchema(methodPointer, notAppliedArguments)
              )
            case atomConstructor: AtomConstructor =>
              val functionInfo =
                FunctionPointer.fromAtomConstructor(atomConstructor)
              val notAppliedArguments = FunctionPointer
                .collectNotAppliedArguments(
                  atomConstructor.getConstructorFunction
                )
                .toVector
              toMethodPointer(functionInfo).map(methodPointer =>
                Api.FunctionSchema(methodPointer, notAppliedArguments)
              )
            case _ =>
              None
          }

          Some(Api.ExpressionUpdate.Payload.Value(warnings, schema))
      }
      payload.foreach { p =>
        ctx.endpoint.sendToClient(
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              Set(
                Api.ExpressionUpdate(
                  value.getExpressionId,
                  Option(value.getType).map(toExpressionType),
                  methodCall,
                  value.getProfilingInfo.map { case e: ExecutionTime =>
                    Api.ProfilingInfo.ExecutionTime(e.getNanoTimeElapsed)
                  }.toVector,
                  value.wasCached(),
                  value.isTypeChanged || value.isFunctionCallChanged,
                  p
                )
              )
            )
          )
        )
      }

      syncState.setExpressionSync(expressionId)
      ctx.state.expressionExecutionState.setExpressionExecuted(expressionId)
      if (methodCall.isDefined) {
        syncState.setMethodPointerSync(expressionId)
      }
    }
  }

  /** Find visualizations for the provided expression value, compute and send
    * the updates.
    *
    * @param contextId the identifier of an execution context
    * @param runtimeCache runtime cache for this execution
    * @param syncState reference to synchronization state
    * @param value the computed value
    * @param ctx the runtime context
    */
  private def sendVisualizationUpdates(
    contextId: Api.ContextId,
    runtimeCache: RuntimeCache,
    syncState: UpdatesSynchronizationState,
    value: ExpressionValue
  )(implicit ctx: RuntimeContext): Unit = {
    val visualizations =
      ctx.contextManager.findVisualizationsForExpression(
        contextId,
        value.getExpressionId
      )
    visualizations.foreach { visualization =>
      if (!syncState.isVisualizationSync(visualization.id)) {
        val v = if (visualization.expressionId == value.getExpressionId) {
          value.getValue
        } else {
          runtimeCache.getAnyValue(visualization.expressionId)
        }
        if (v != null && !VisualizationResult.isInterruptedException(v)) {
          executeAndSendVisualizationUpdate(
            contextId,
            runtimeCache,
            syncState,
            visualization,
            value.getExpressionId,
            v
          )
        }
      }
    }
  }

  private def executeVisualization(
    contextId: Api.ContextId,
    runtimeCache: RuntimeCache,
    visualization: Visualization,
    expressionId: UUID,
    expressionValue: AnyRef
  )(implicit ctx: RuntimeContext): Either[Throwable, AnyRef] =
    Try {
      logger.trace(
        "Executing visualization [{}] on expression [{}] of [{}]...",
        visualization.id,
        expressionId,
        Try(TypeOfNode.getUncached.findTypeOrError(expressionValue))
          .getOrElse(expressionValue.getClass)
      )
      val holder = ctx.contextManager.getVisualizationHolder(contextId)

      val makeCall = new Supplier[CompletionStage[AnyRef]] {
        override def get(): CompletionStage[AnyRef] = {
          ctx.executionService.callFunctionWithInstrument(
            holder,
            visualization.cache,
            runtimeCache,
            visualization.module,
            visualization.callback,
            expressionValue +: visualization.arguments: _*
          )
        }
      }

      val pending = if (runtimeCache != null) {
        val processUUID = new Consumer[UUID] {
          override def accept(id: Api.ContextId): Unit = {
            logger.trace(
              "Associating visualization [{}] with additional ID [{}]",
              visualization.id,
              id
            )
            holder.upsert(visualization, id)
          }
        }
        runtimeCache.runQuery(processUUID, makeCall)
      } else {
        makeCall.get()
      }
      val visualizationResult = pending.toCompletableFuture.get()
      logger.trace(
        "Visualization {} on expression {} resulted in {}",
        visualization.id,
        expressionId,
        visualizationResult
      )
      visualizationResult
    }.toEither

  /** Compute the visualization of the expression value and send an update.
    *
    * @param contextId an identifier of an execution context
    * @param visualizationId the id of the visualization
    * @param expressionId the id of expression to visualise
    * @param expressionValue the value of expression to visualise
    * @param ctx the runtime context
    */
  def sendVisualizationUpdate(
    visualizationResult: Either[Throwable, AnyRef],
    contextId: Api.ContextId,
    syncState: UpdatesSynchronizationState,
    visualizationId: UUID,
    expressionId: UUID,
    expressionValue: AnyRef
  )(implicit ctx: RuntimeContext): Unit = {
    visualizationResultToBytes(visualizationResult) match {
      case Left(_: ThreadInterruptedException) =>

      case Left(throwable) =>
        val error = throwable match {
          case e: ExecutionException if e.getCause != null => e.getCause
          case _                                           => throwable
        }
        val message =
          Option(error.getMessage).getOrElse(error.getClass.getSimpleName)
        if (!TypesGen.isPanicSentinel(expressionValue)) {
          val typeOfNode =
            ctx.executionService
              .typeOfValue(expressionValue)
              .toCompletableFuture
              .get()

          logger.warn(
            "Execution of visualization [{}] on value [{} of type {}] failed. {} | {} | {}",
            visualizationId,
            expressionId,
            typeOfNode,
            message,
            expressionValue,
            error
          )
          error match {
            case p: AbstractTruffleException if p.getLocation() != null => {
              p.getLocation().getEncapsulatingSourceSection() match {
                case ss: SourceSection =>
                  logger.warn(
                    s"Error at ${ss.getCharIndex()}-${ss
                      .getCharEndIndex()} in ${ss.getSource.getPath} (e.g. `${ss
                      .getCharacters()}`) of visualization $visualizationId",
                    p
                  )
                case _ =>
              }
            }
            case _ =>
          }
        }
        syncState.runAndSetVisualizationSync(
          visualizationId,
          () => {
            ctx.endpoint.sendToClient(
              Api.Response(
                Api.VisualizationEvaluationFailed(
                  Api
                    .VisualizationContext(
                      visualizationId,
                      contextId,
                      expressionId
                    ),
                  message,
                  getDiagnosticOutcome(error)
                )
              )
            )
          }
        )

      case Right(data) =>
        logger.trace(
          "Visualization executed [{}].",
          expressionId
        )
        syncState.runAndSetVisualizationSync(
          visualizationId,
          () => {
            ctx.endpoint.sendToClient(
              Api.Response(
                Api.VisualizationUpdate(
                  Api.VisualizationContext(
                    visualizationId,
                    contextId,
                    expressionId
                  ),
                  data
                )
              )
            )
          }
        )
    }
  }

  /** Compute the visualization of the expression value and send an update.
    *
    * @param contextId an identifier of an execution context
    * @param runtimeCache runtime cache for this execution
    * @param syncState reference to synchronization state
    * @param visualization the visualization data
    * @param expressionId the id of expression to visualise
    * @param expressionValue the value of expression to visualise
    * @param ctx the runtime context
    */
  def executeAndSendVisualizationUpdate(
    contextId: Api.ContextId,
    runtimeCache: RuntimeCache,
    syncState: UpdatesSynchronizationState,
    visualization: Visualization,
    expressionId: UUID,
    expressionValue: AnyRef
  )(implicit ctx: RuntimeContext): Unit = {
    val visualizationResult =
      executeVisualization(
        contextId,
        runtimeCache,
        visualization,
        expressionId,
        expressionValue
      )
    sendVisualizationUpdate(
      visualizationResult,
      contextId,
      syncState,
      visualization.id,
      expressionId,
      expressionValue
    )
  }

  /** Convert the result of Enso visualization function to a byte array.
    *
    * @param visualizationResult the result of Enso visualization function
    * @return either a byte array representing the visualization result or an
    *         error
    */
  private def visualizationResultToBytes(
    visualizationResult: Either[Throwable, AnyRef]
  ): Either[Throwable, Array[Byte]] = {
    visualizationResult.flatMap { value =>
      Option(VisualizationResult.visualizationResultToBytes(value)).toRight(
        new VisualizationException(
          s"Cannot encode ${value.getClass} to byte array."
        )
      )
    }
  }

  /** Extract the method call information from the provided expression value.
    *
    * @param value the expression value.
    * @return the method call info
    */
  private def toMethodCall(value: ExpressionValue): Option[Api.MethodCall] = {
    // While hiding the cached method pointer info for evaluated values, it is a
    // good idea to return the cached method pointer value for dataflow errors
    // (the one before the value turned into a dataflow error) to continue
    // displaying widgets on child nodes even after those nodes become errors.
    def notCachedAndNotDataflowError: Boolean =
      !value.wasCached() && !value.getValue.isInstanceOf[DataflowError]

    val isPanicType =
      value.getType != null && Types.isPanic(value.getType.visibleType())
    for {
      call <-
        if (isPanicType || notCachedAndNotDataflowError)
          Option(value.getCallInfo)
        else Option(value.getCallInfo).orElse(Option(value.getCachedCallInfo))
      methodPointer <- toMethodPointer(call.functionPointer)
    } yield {
      Api.MethodCall(methodPointer, call.notAppliedArguments.toVector)
    }
  }

  /** Extract the method pointer information form the provided runtime function
    * pointer.
    *
    * @param functionPointer the runtime function pointer
    * @return the extracted method pointer
    */
  private def toMethodPointer(
    functionPointer: FunctionPointer
  ): Option[Api.MethodPointer] =
    for {
      moduleName   <- Option(functionPointer.moduleName)
      typeName     <- Option(functionPointer.typeName)
      functionName <- Option(functionPointer.functionName)
    } yield Api.MethodPointer(
      moduleName.toString,
      typeName.toString.stripSuffix(TypeSuffix),
      functionName
    )

  /** Extract the expression type information from the provided type info.
    *
    * @param typeInfo the runtime type info
    * @return the appropriate expression type
    */
  private def toExpressionType(typeInfo: TypeInfo): Api.ExpressionType =
    Api.ExpressionType(
      typeInfo.visibleType().toVector,
      typeInfo.hiddenType().toVector
    )

  /** Find source file path by the module name.
    *
    * @param module the module name
    * @param ctx the runtime context
    * @return the source file path
    */
  private def findFileByModuleName(
    module: String
  )(implicit ctx: RuntimeContext): Option[File] =
    for {
      module <- ctx.executionService.getContext.findModule(module).toScala
      path   <- Option(module.getPath)
    } yield new File(path)

  /** An execution frame.
    *
    * @param item the executionitem
    * @param cache the cache of this stack frame
    * @param syncState the synchronization state of message updates
    */
  sealed private case class ExecutionFrame(
    item: ExecutionItem,
    cache: RuntimeCache,
    syncState: UpdatesSynchronizationState
  )

  /** A local call frame defined by the expression id.
    *
    * @param expressionId the id of the expression
    * @param cache the cache of this frame
    * @param syncState the synchronization state of message updates
    */
  sealed private case class LocalCallFrame(
    expressionId: UUID,
    cache: RuntimeCache,
    syncState: UpdatesSynchronizationState
  )

  private val TypeSuffix = ".type"
}
