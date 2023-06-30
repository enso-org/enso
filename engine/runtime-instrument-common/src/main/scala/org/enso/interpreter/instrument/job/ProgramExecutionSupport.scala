package org.enso.interpreter.instrument.job

import cats.implicits._
import com.oracle.truffle.api.exception.AbstractTruffleException
import org.enso.interpreter.instrument.IdExecutionService.{
  ExpressionCall,
  ExpressionValue
}
import org.enso.interpreter.instrument.execution.{
  Completion,
  ErrorResolver,
  LocationResolver,
  RuntimeContext
}
import org.enso.interpreter.instrument.profiling.ExecutionTime
import org.enso.interpreter.instrument._
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode.FunctionCall
import org.enso.interpreter.runtime.`type`.Types
import org.enso.interpreter.runtime.control.ThreadInterruptedException
import org.enso.interpreter.runtime.error.{
  DataflowError,
  PanicSentinel,
  WarningsLibrary
}
import org.enso.interpreter.service.error._
import org.enso.polyglot.LanguageInfo
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.{ContextId, ExecutionResult}

import java.io.File
import java.util.UUID
import java.util.function.Consumer
import java.util.logging.Level
import scala.jdk.OptionConverters._

/** Provides support for executing Enso code. Adds convenient methods to
  * run Enso programs in a Truffle context.
  */
object ProgramExecutionSupport {

  /** Runs an Enso program.
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
        sendVisualizationUpdates(contextId, executionFrame.syncState, value)
      }
    }

    val onComputedValueCallback: Consumer[ExpressionValue] = { value =>
      if (callStack.isEmpty) {
        logger.log(Level.FINEST, s"ON_COMPUTED ${value.getExpressionId}")
        sendExpressionUpdate(contextId, executionFrame.syncState, value)
        sendVisualizationUpdates(contextId, executionFrame.syncState, value)
      }
    }

    val onExceptionalCallback: Consumer[Exception] = { value =>
      logger.log(Level.FINEST, s"ON_ERROR $value")
      sendErrorUpdate(contextId, value)
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
        ctx.executionService.execute(
          module.toString,
          cons.item,
          function,
          cache,
          methodCallsCache,
          syncState,
          callStack.headOption.map(_.expressionId).orNull,
          callablesCallback,
          onComputedValueCallback,
          onCachedValueCallback,
          onExceptionalCallback
        )
      case ExecutionFrame(
            ExecutionItem.CallData(expressionId, callData),
            cache,
            syncState
          ) =>
        val module =
          ctx.executionService.getContext
            .findModuleByExpressionId(expressionId)
            .orElseThrow(() =>
              new ModuleNotFoundForExpressionIdException(expressionId)
            )
        ctx.executionService.execute(
          module,
          callData,
          cache,
          methodCallsCache,
          syncState,
          callStack.headOption.map(_.expressionId).orNull,
          callablesCallback,
          onComputedValueCallback,
          onCachedValueCallback,
          onExceptionalCallback
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

  /** Runs an Enso program.
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
    val executionResult = for {
      stackItem <- Either.fromOption(
        explicitCallOpt,
        Api.ExecutionResult.Failure("Execution stack is empty.", None)
      )
      _ <-
        Either
          .catchNonFatal(executeProgram(contextId, stackItem, localCalls))
          .leftMap(onExecutionError(stackItem.item, _))
    } yield ()
    logger.log(Level.FINEST, s"Execution finished: $executionResult")
    executionResult.fold(Some(_), _ => None)
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
  )(implicit ctx: RuntimeContext): Api.ExecutionResult = {
    val itemName = item match {
      case ExecutionItem.Method(_, _, function) => function
      case ExecutionItem.CallData(_, call)      => call.getFunction.getName
    }
    val executionUpdate = getExecutionOutcome(error)
    val reason          = VisualizationResult.findExceptionMessage(error)
    def onFailure() = error match {
      case _: ThreadInterruptedException =>
        val message = s"Execution of function $itemName interrupted."
        ctx.executionService.getLogger.log(Level.FINE, message)
        ExecutionResult.Diagnostic.warning(message, None)
      case _ =>
        val message = s"Execution of function $itemName failed ($reason)."
        ctx.executionService.getLogger.log(Level.WARNING, message, error)
        ExecutionResult.Failure(message, None)
    }
    executionUpdate.getOrElse(onFailure())
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
        // The empty language is allowed because `getLanguage` returns null when
        // the error originates in builtin node.
        if Option(ctx.executionService.getLanguage(ex))
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

    case ex: ServiceException =>
      Api.ExecutionResult.Failure(ex.getMessage, None)
  }

  private def sendErrorUpdate(contextId: ContextId, error: Exception)(implicit
    ctx: RuntimeContext
  ): Unit = {
    ctx.endpoint.sendToClient(
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(
            getDiagnosticOutcome.applyOrElse(
              error,
              (ex: Exception) =>
                Api.ExecutionResult.Diagnostic.error(ex.getMessage)
            )
          )
        )
      )
    )
  }

  private def sendExpressionUpdate(
    contextId: ContextId,
    syncState: UpdatesSynchronizationState,
    value: ExpressionValue
  )(implicit ctx: RuntimeContext): Unit = {
    val expressionId  = value.getExpressionId
    val methodPointer = toMethodCall(value)
    if (
      !syncState.isExpressionSync(expressionId) ||
      (
        methodPointer.isDefined && !syncState.isMethodPointerSync(expressionId)
      ) ||
      Types.isPanic(value.getType)
    ) {
      val payload = value.getValue match {
        case sentinel: PanicSentinel =>
          Api.ExpressionUpdate.Payload
            .Panic(
              ctx.executionService.getExceptionMessage(sentinel.getPanic),
              ErrorResolver.getStackTrace(sentinel).flatMap(_.expressionId)
            )
        case error: DataflowError =>
          Api.ExpressionUpdate.Payload.DataflowError(
            ErrorResolver.getStackTrace(error).flatMap(_.expressionId)
          )
        case panic: AbstractTruffleException =>
          Api.ExpressionUpdate.Payload
            .Panic(
              VisualizationResult.findExceptionMessage(panic),
              ErrorResolver.getStackTrace(panic).flatMap(_.expressionId)
            )
        case _ =>
          if (WarningsLibrary.getUncached.hasWarnings(value.getValue)) {
            val warnings =
              WarningsLibrary.getUncached.getWarnings(value.getValue, null)
            val warningsCount = warnings.length
            val warning =
              if (warningsCount == 1) {
                Option(
                  ctx.executionService.toDisplayString(warnings(0).getValue)
                )
              } else {
                None
              }
            Api.ExpressionUpdate.Payload.Value(
              Some(
                Api.ExpressionUpdate.Payload.Value
                  .Warnings(
                    warningsCount,
                    warning,
                    WarningsLibrary.getUncached.isLimitReached(value.getValue)
                  )
              )
            )
          } else {
            Api.ExpressionUpdate.Payload.Value()
          }
      }
      ctx.endpoint.sendToClient(
        Api.Response(
          Api.ExpressionUpdates(
            contextId,
            Set(
              Api.ExpressionUpdate(
                value.getExpressionId,
                Option(value.getType),
                methodPointer,
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
      if (methodPointer.isDefined) {
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
  @com.oracle.truffle.api.CompilerDirectives.TruffleBoundary
  private def sendVisualizationUpdates(
    contextId: ContextId,
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
        sendVisualizationUpdate(
          contextId,
          syncState,
          visualization,
          value.getExpressionId,
          value.getValue
        )
      }
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
  def sendVisualizationUpdate(
    contextId: ContextId,
    syncState: UpdatesSynchronizationState,
    visualization: Visualization,
    expressionId: UUID,
    expressionValue: AnyRef
  )(implicit ctx: RuntimeContext): Unit = {
    val errorOrVisualizationData =
      Either
        .catchNonFatal {
          ctx.executionService.getLogger.log(
            Level.FINE,
            s"Executing visualization ${visualization.expressionId}"
          )
          ctx.executionService.callFunctionWithInstrument(
            visualization.cache,
            visualization.module,
            visualization.callback,
            expressionValue +: visualization.arguments: _*
          )
        }
        .flatMap(visualizationResultToBytes)
    val result = errorOrVisualizationData match {
      case Left(_: ThreadInterruptedException) =>
        ctx.executionService.getLogger.log(
          Level.FINE,
          s"Visualization thread interrupted ${visualization.expressionId}."
        )
        Completion.Interrupted

      case Left(error) =>
        val message =
          Option(error.getMessage).getOrElse(error.getClass.getSimpleName)
        ctx.executionService.getLogger.log(
          Level.WARNING,
          s"Visualization evaluation failed: $message."
        )
        ctx.endpoint.sendToClient(
          Api.Response(
            Api.VisualizationEvaluationFailed(
              contextId,
              visualization.id,
              expressionId,
              message,
              getDiagnosticOutcome.lift(error)
            )
          )
        )
        Completion.Done

      case Right(data) =>
        ctx.executionService.getLogger.log(
          Level.FINEST,
          s"Visualization computed ${visualization.expressionId}."
        )
        ctx.endpoint.sendToClient(
          Api.Response(
            Api.VisualizationUpdate(
              Api.VisualizationContext(
                visualization.id,
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

  /** Convert the result of Enso visualization function to a byte array.
    *
    * @param value the result of Enso visualization function
    * @return either a byte array representing the visualization result or an
    *         error
    */
  private def visualizationResultToBytes(
    value: AnyRef
  ): Either[VisualizationException, Array[Byte]] = {
    Option(VisualizationResult.visualizationResultToBytes(value)).toRight(
      new VisualizationException(
        s"Cannot encode ${value.getClass} to byte array."
      )
    )
  }

  /** Extract the method call information from the provided expression value.
    *
    * @param value the expression value.
    * @return the method call info
    */
  private def toMethodCall(value: ExpressionValue): Option[Api.MethodCall] =
    for {
      call       <- Option(value.getCallInfo).orElse(Option(value.getCachedCallInfo))
      moduleName <- Option(call.getModuleName)
      typeName   <- Option(call.getTypeName)
    } yield {
      Api.MethodCall(
        methodPointer = Api.MethodPointer(
          moduleName.toString,
          typeName.toString.stripSuffix(TypeSuffix),
          call.getFunctionName
        ),
        notAppliedArguments = call.getNotAppliedArguments.toVector
      )
    }

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
