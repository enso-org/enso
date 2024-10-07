package org.enso.interpreter.instrument.job

import com.oracle.truffle.api.exception.AbstractTruffleException
import org.enso.interpreter.instrument.{
  InstrumentFrame,
  MethodCallsCache,
  RuntimeCache,
  UpdatesSynchronizationState,
  Visualization,
  WarningPreview
}
import org.enso.interpreter.instrument.execution.{
  Completion,
  ErrorResolver,
  LocationResolver,
  RuntimeContext
}
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
import org.enso.common.LanguageInfo
import org.enso.interpreter.runtime.warning.{
  Warning,
  WarningsLibrary,
  WithWarnings
}
import org.enso.polyglot.debugger.ExecutedVisualization
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.{ContextId, ExecutionResult}

import java.io.File
import java.util.UUID
import java.util.function.Consumer
import java.util.logging.Level

import scala.jdk.OptionConverters.RichOptional
import scala.util.Try

/** Provides support for executing Enso code. Adds convenient methods to
  * run Enso programs in a Truffle context.
  */
object ProgramExecutionSupport {

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
    val logger           = ctx.executionService.getLogger
    val methodCallsCache = new MethodCallsCache
    var enterables       = Map[UUID, FunctionCall]()

    val onCachedMethodCallCallback: Consumer[ExpressionValue] = { value =>
      logger.log(Level.FINEST, s"ON_CACHED_CALL ${value.getExpressionId}")
      sendExpressionUpdate(contextId, executionFrame.syncState, value)
    }

    val onCachedValueCallback: Consumer[ExpressionValue] = { value =>
      if (callStack.isEmpty) {
        logger.log(Level.FINEST, s"ON_CACHED_VALUE ${value.getExpressionId}")
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
        logger.log(Level.FINEST, s"ON_COMPUTED ${value.getExpressionId}")
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

    executionFrame match {
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

    callStack match {
      case Nil =>
        val notExecuted =
          methodCallsCache.getNotExecuted(executionFrame.cache.getCalls)
        notExecuted.forEach { expressionId =>
          val expressionType = executionFrame.cache.getType(expressionId)
          val expressionCall = executionFrame.cache.getCall(expressionId)
          onCachedMethodCallCallback.accept(
            new ExpressionValue(
              expressionId,
              null,
              expressionType,
              expressionType,
              expressionCall,
              expressionCall,
              Array(ExecutionTime.empty()),
              true
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
    val logger = ctx.executionService.getLogger
    logger.log(Level.FINEST, s"Run program $contextId")
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
    logger.log(Level.FINEST, s"Execution finished: $executionResult")
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
    error: Throwable
  )(implicit ctx: RuntimeContext): Option[Api.ExecutionResult] = {
    val itemName = item match {
      case ExecutionItem.Method(_, _, function) => function
      case ExecutionItem.CallData(_, call)      => call.getFunction.getName
    }
    val executionUpdate = getExecutionOutcome(error)
    val reason          = VisualizationResult.findExceptionMessage(error)
    def onFailure(): Option[Api.ExecutionResult] = error match {
      case _: ThreadInterruptedException =>
        val message = s"Execution of function $itemName interrupted."
        ctx.executionService.getLogger.log(Level.FINE, message)
        None
      case _ =>
        val message = s"Execution of function $itemName failed ($reason)."
        ctx.executionService.getLogger.log(Level.WARNING, message, error)
        Some(ExecutionResult.Failure(message, None))
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
  )(implicit ctx: RuntimeContext): Option[Api.ExecutionResult] =
    getDiagnosticOutcome.orElse(getFailureOutcome).lift(t)

  /** Extract diagnostic information from the provided exception. */
  def getDiagnosticOutcome(implicit
    ctx: RuntimeContext
  ): PartialFunction[Throwable, Api.ExecutionResult.Diagnostic] = {
    case ex: AbstractTruffleException
        // exit exception is special, and handled as failure rather than Diagnostics.
        if !ctx.executionService.isExitException(ex) &&
        // The empty language is allowed because `getLanguage` returns null when
        // the error originates in builtin node.
        Option(ctx.executionService.getLanguage(ex))
          .forall(_ == LanguageInfo.ID) =>
      val section = Option(ctx.executionService.getSourceLocation(ex))
      val source  = section.flatMap(sec => Option(sec.getSource))
      Api.ExecutionResult.Diagnostic.error(
        VisualizationResult.findExceptionMessage(ex),
        source.flatMap(src => findFileByModuleName(src.getName)),
        section.map(LocationResolver.sectionToRange),
        section
          .flatMap(LocationResolver.getExpressionId(_))
          .map(_.externalId),
        ErrorResolver.getStackTrace(ex)
      )
  }

  /** Extract information about the failure from the provided exception. */
  private def getFailureOutcome(implicit
    ctx: RuntimeContext
  ): PartialFunction[Throwable, Api.ExecutionResult.Failure] = {
    case ex: TypeNotFoundException =>
      Api.ExecutionResult.Failure(
        ex.getMessage,
        findFileByModuleName(ex.getModule)
      )

    case ex: MethodNotFoundException =>
      Api.ExecutionResult.Failure(
        ex.getMessage,
        findFileByModuleName(ex.getModule)
      )

    case exitEx: AbstractTruffleException
        if ctx.executionService.isExitException(exitEx) =>
      val section = Option(ctx.executionService.getSourceLocation(exitEx))
      val source  = section.flatMap(sec => Option(sec.getSource))
      val file    = source.flatMap(src => findFileByModuleName(src.getName))
      Api.ExecutionResult.Failure(
        exitEx.getMessage,
        file
      )

    case ex: ServiceException =>
      Api.ExecutionResult.Failure(ex.getMessage, None)
  }

  private def sendExpressionUpdate(
    contextId: ContextId,
    syncState: UpdatesSynchronizationState,
    value: ExpressionValue
  )(implicit ctx: RuntimeContext): Unit = {
    val expressionId = value.getExpressionId
    val methodCall   = toMethodCall(value)
    if (
      !syncState.isExpressionSync(expressionId) ||
      (
        methodCall.isDefined && !syncState.isMethodPointerSync(
          expressionId
        )
      ) ||
      Types.isPanic(value.getType)
    ) {
      val payload = value.getValue match {
        case sentinel: PanicSentinel =>
          Some(
            Api.ExpressionUpdate.Payload
              .Panic(
                ctx.executionService.getExceptionMessage(sentinel.getPanic),
                ErrorResolver.getStackTrace(sentinel).flatMap(_.expressionId)
              )
          )
        case error: DataflowError =>
          Some(
            Api.ExpressionUpdate.Payload.DataflowError(
              ErrorResolver.getStackTrace(error).flatMap(_.expressionId)
            )
          )
        case panic: AbstractTruffleException =>
          if (!VisualizationResult.isInterruptedException(panic)) {
            Some(
              Api.ExpressionUpdate.Payload.Panic(
                VisualizationResult.findExceptionMessage(panic),
                ErrorResolver.getStackTrace(panic).flatMap(_.expressionId)
              )
            )
          } else {
            ctx.executionService.getLogger
              .log(Level.FINE, "computation of expression has been interrupted")
            None
          }
        case warnings: WithWarnings
            if warnings.getValue.isInstanceOf[DataflowError] =>
          Some(
            Api.ExpressionUpdate.Payload.DataflowError(
              ErrorResolver
                .getStackTrace(warnings.getValue.asInstanceOf[DataflowError])
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
                    WarningPreview.execute(warnings(0).getValue)
                  ).toEither
                    .fold(
                      error => {
                        ctx.executionService.getLogger.log(
                          Level.SEVERE,
                          "Failed to execute warning preview of expression [{0}].",
                          Array[Object](expressionId, error)
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
                  Option(value.getType),
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
    * @param value the computed value
    * @param ctx the runtime context
    */
  private def sendVisualizationUpdates(
    contextId: ContextId,
    runtimeCache: RuntimeCache,
    syncState: UpdatesSynchronizationState,
    value: ExpressionValue
  )(implicit ctx: RuntimeContext): Unit = {
    if (!syncState.isVisualizationSync(value.getExpressionId)) {
      val visualizations =
        ctx.contextManager.findVisualizationForExpression(
          contextId,
          value.getExpressionId
        )
      visualizations.foreach { visualization =>
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
    contextId: ContextId,
    runtimeCache: RuntimeCache,
    visualization: Visualization,
    expressionId: UUID,
    expressionValue: AnyRef
  )(implicit ctx: RuntimeContext): Either[Throwable, AnyRef] =
    Try {
      val logger = ctx.executionService.getLogger
      logger.log(
        Level.FINEST,
        "Executing visualization [{0}] on expression [{1}] of [{2}]...",
        Array[Object](
          visualization.id,
          expressionId,
          Try(TypeOfNode.getUncached.execute(expressionValue))
            .getOrElse(expressionValue.getClass)
        )
      )
      val holder = ctx.contextManager.getVisualizationHolder(contextId)

      def makeCall(): AnyRef =
        ctx.executionService.callFunctionWithInstrument(
          holder,
          visualization.cache,
          runtimeCache,
          visualization.module,
          visualization.callback,
          expressionValue +: visualization.arguments: _*
        )

      if (runtimeCache != null) {
        def processUUID(id: UUID): Unit = {
          logger.log(
            Level.WARNING,
            "Associating visualization [{0}] with additional ID [{1}]",
            Array[Object](
              visualization.id,
              id
            )
          )

          holder.upsert(visualization, id)
        }
        runtimeCache.runQuery(processUUID, () => makeCall())
      } else {
        makeCall()
      }
    }.toEither

  /** Compute the visualization of the expression value and send an update.
    *
    * @param contextId an identifier of an execution context
    * @param visualizationId the id of the visualization
    * @param expressionId the id of expression to visualise
    * @param expressionValue the value of expression to visualise
    * @param ctx the runtime context
    */
  private def sendVisualizationUpdate(
    visualizationResult: Either[Throwable, AnyRef],
    contextId: ContextId,
    syncState: UpdatesSynchronizationState,
    visualizationId: UUID,
    expressionId: UUID,
    expressionValue: AnyRef
  )(implicit ctx: RuntimeContext): Unit = {
    val result = visualizationResultToBytes(visualizationResult) match {
      case Left(_: ThreadInterruptedException) =>
        Completion.Interrupted

      case Left(error) =>
        val message =
          Option(error.getMessage).getOrElse(error.getClass.getSimpleName)
        if (!TypesGen.isPanicSentinel(expressionValue)) {
          val typeOfNode =
            Option(TypeOfNode.getUncached.execute(expressionValue))
              .getOrElse(expressionValue.getClass)
          ctx.executionService.getLogger.log(
            Level.WARNING,
            "Execution of visualization [{0}] on value [{1}] of [{2}] failed. {3} | {4} | {5}",
            Array[Object](
              visualizationId,
              expressionId,
              typeOfNode,
              message,
              expressionValue,
              error
            )
          )
        }
        ctx.endpoint.sendToClient(
          Api.Response(
            Api.VisualizationEvaluationFailed(
              Api
                .VisualizationContext(visualizationId, contextId, expressionId),
              message,
              getDiagnosticOutcome.lift(error)
            )
          )
        )
        Completion.Done

      case Right(data) =>
        ctx.executionService.getLogger.log(
          Level.FINEST,
          s"Visualization executed [{0}].",
          expressionId
        )
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
        Completion.Done
    }
    if (result != Completion.Interrupted) {
      syncState.setVisualizationSync(expressionId)
    }
  }

  /** Compute the visualization of the expression value and send an update.
    *
    * @param contextId an identifier of an execution context
    * @param visualization the visualization data
    * @param expressionId the id of expression to visualise
    * @param expressionValue the value of expression to visualise
    * @param ctx the runtime context
    */
  def executeAndSendVisualizationUpdate(
    contextId: ContextId,
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
    for {
      call <-
        if (Types.isPanic(value.getType) || notCachedAndNotDataflowError)
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
