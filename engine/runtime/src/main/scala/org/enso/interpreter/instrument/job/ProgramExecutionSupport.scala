package org.enso.interpreter.instrument.job

import cats.implicits._
import com.oracle.truffle.api.exception.AbstractTruffleException
import org.enso.interpreter.instrument.IdExecutionInstrument.{
  ExpressionCall,
  ExpressionValue
}
import org.enso.interpreter.instrument.execution.{
  ErrorResolver,
  LocationResolver,
  RuntimeContext
}
import org.enso.interpreter.instrument.job.ProgramExecutionSupport.{
  ExecutionFrame,
  LocalCallFrame
}
import org.enso.interpreter.instrument.profiling.ExecutionTime
import org.enso.interpreter.instrument.{
  InstrumentFrame,
  MethodCallsCache,
  RuntimeCache,
  Visualisation
}
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode.FunctionCall
import org.enso.interpreter.runtime.data.text.Text
import org.enso.interpreter.runtime.error.{DataflowError, PanicSentinel}
import org.enso.interpreter.service.error.{
  ConstructorNotFoundException,
  MethodNotFoundException,
  ServiceException
}
import org.enso.polyglot.LanguageInfo
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.ContextId

import java.io.File
import java.util.function.Consumer
import java.util.logging.Level
import java.util.{Objects, UUID}
import scala.jdk.OptionConverters._

/** Provides support for executing Enso code. Adds convenient methods to
  * run Enso programs in a Truffle context.
  */
trait ProgramExecutionSupport {

  /** Runs an Enso program.
    *
    * @param executionFrame an execution frame
    * @param callStack a call stack
    * @param onCachedMethodCallCallback a listener of cached method calls
    * @param onComputedCallback a listener of computed values
    * @param onCachedCallback a listener of cached values
    * @param onExceptionalCallback the consumer of the exceptional events.
    */
  @scala.annotation.tailrec
  final private def executeProgram(
    executionFrame: ExecutionFrame,
    callStack: List[LocalCallFrame],
    onCachedMethodCallCallback: Consumer[ExpressionValue],
    onComputedCallback: Consumer[ExpressionValue],
    onCachedCallback: Consumer[ExpressionValue],
    onExceptionalCallback: Consumer[Exception]
  )(implicit ctx: RuntimeContext): Unit = {
    val methodCallsCache = new MethodCallsCache
    var enterables       = Map[UUID, FunctionCall]()
    val computedCallback: Consumer[ExpressionValue] =
      if (callStack.isEmpty) onComputedCallback else _ => ()
    val callablesCallback: Consumer[ExpressionCall] = fun =>
      if (callStack.headOption.exists(_.expressionId == fun.getExpressionId)) {
        enterables += fun.getExpressionId -> fun.getCall
      }
    executionFrame match {
      case ExecutionFrame(
            ExecutionItem.Method(module, cons, function),
            cache
          ) =>
        ctx.executionService.execute(
          module.toString,
          cons.item,
          function,
          cache,
          methodCallsCache,
          callStack.headOption.map(_.expressionId).orNull,
          callablesCallback,
          computedCallback,
          onCachedCallback,
          onExceptionalCallback
        )
      case ExecutionFrame(ExecutionItem.CallData(callData), cache) =>
        ctx.executionService.execute(
          callData,
          cache,
          methodCallsCache,
          callStack.headOption.map(_.expressionId).orNull,
          callablesCallback,
          computedCallback,
          onCachedCallback,
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
            executeProgram(
              ExecutionFrame(ExecutionItem.CallData(call), item.cache),
              tail,
              onCachedMethodCallCallback,
              onComputedCallback,
              onCachedCallback,
              onExceptionalCallback
            )
          case None =>
            ()
        }
    }
  }

  /** Runs an Enso program.
    *
    * @param contextId an identifier of an execution context
    * @param stack a call stack
    * @param updatedVisualisations a list of updated visualisations
    * @param sendMethodCallUpdates a flag to send all the method calls of the
    * executed frame as a value updates
    * @param ctx a runtime context
    * @return a diagnostic message
    */
  final def runProgram(
    contextId: Api.ContextId,
    stack: List[InstrumentFrame],
    updatedVisualisations: Seq[Api.ExpressionId],
    sendMethodCallUpdates: Boolean
  )(implicit ctx: RuntimeContext): Option[Api.ExecutionResult] = {
    val logger = ctx.executionService.getLogger
    logger.log(
      Level.FINEST,
      s"Run program updatedVisualisations=$updatedVisualisations sendMethodCallUpdates=$sendMethodCallUpdates"
    )
    @scala.annotation.tailrec
    def unwind(
      stack: List[InstrumentFrame],
      explicitCalls: List[ExecutionFrame],
      localCalls: List[LocalCallFrame]
    ): (Option[ExecutionFrame], List[LocalCallFrame]) =
      stack match {
        case Nil =>
          (explicitCalls.lastOption, localCalls)
        case List(InstrumentFrame(call: Api.StackItem.ExplicitCall, cache)) =>
          (Some(ExecutionFrame(ExecutionItem.Method(call), cache)), localCalls)
        case InstrumentFrame(Api.StackItem.LocalCall(id), cache) :: xs =>
          unwind(xs, explicitCalls, LocalCallFrame(id, cache) :: localCalls)
      }

    val onCachedMethodCallCallback: Consumer[ExpressionValue] = { value =>
      logger.log(Level.FINEST, s"ON_CACHED_CALL ${value.getExpressionId}")
      sendValueUpdate(contextId, value, sendMethodCallUpdates)
    }

    val onCachedValueCallback: Consumer[ExpressionValue] = { value =>
      if (updatedVisualisations.contains(value.getExpressionId)) {
        logger.log(Level.FINEST, s"ON_CACHED_VALUE ${value.getExpressionId}")
        fireVisualisationUpdates(contextId, value)
      }
    }

    val onComputedValueCallback: Consumer[ExpressionValue] = { value =>
      logger.log(Level.FINEST, s"ON_COMPUTED ${value.getExpressionId}")
      sendValueUpdate(contextId, value, sendMethodCallUpdates)
      fireVisualisationUpdates(contextId, value)
    }

    val onExceptionalCallback: Consumer[Exception] = { value =>
      logger.log(Level.FINEST, s"ON_ERROR $value")
      sendErrorUpdate(contextId, value)
    }

    val (explicitCallOpt, localCalls) = unwind(stack, Nil, Nil)
    val executionResult = for {
      stackItem <- Either.fromOption(
        explicitCallOpt,
        Api.ExecutionResult.Failure("Execution stack is empty.", None)
      )
      _ <-
        Either
          .catchNonFatal(
            executeProgram(
              stackItem,
              localCalls,
              onCachedMethodCallCallback,
              onComputedValueCallback,
              onCachedValueCallback,
              onExceptionalCallback
            )
          )
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
      case ExecutionItem.CallData(call)         => call.getFunction.getName
    }
    val executionUpdate = getExecutionOutcome(error)
    executionUpdate match {
      case Some(_) =>
        ctx.executionService.getLogger
          .log(Level.FINEST, s"Error executing a function $itemName.")
      case None =>
        ctx.executionService.getLogger
          .log(Level.FINEST, s"Error executing a function $itemName.", error)
    }
    executionUpdate.getOrElse(
      Api.ExecutionResult
        .Failure(s"Error in function $itemName.", None)
    )
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
    t match {
      case ex: AbstractTruffleException
          if Option(ctx.executionService.getLanguage(ex))
            .forall(_ == LanguageInfo.ID) =>
        val section = Option(ctx.executionService.getSourceLocation(ex))
        Some(
          Api.ExecutionResult.Diagnostic.error(
            ex.getMessage,
            section.flatMap(sec => findFileByModuleName(sec.getSource.getName)),
            section.map(LocationResolver.sectionToRange),
            section
              .flatMap(LocationResolver.getExpressionId(_))
              .map(_.externalId),
            ErrorResolver.getStackTrace(ex)
          )
        )

      case ex: ConstructorNotFoundException =>
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
  }

  private def sendErrorUpdate(contextId: ContextId, error: Exception)(implicit
    ctx: RuntimeContext
  ): Unit = {
    ctx.endpoint.sendToClient(
      Api.Response(
        Api.ExecutionUpdate(
          contextId,
          Seq(Api.ExecutionResult.Diagnostic.error(error.getMessage))
        )
      )
    )
  }

  private def sendValueUpdate(
    contextId: ContextId,
    value: ExpressionValue,
    sendMethodCallUpdates: Boolean
  )(implicit ctx: RuntimeContext): Unit = {
    val methodPointer = toMethodPointer(value)
    if (
      (sendMethodCallUpdates && methodPointer.isDefined) ||
      !Objects.equals(value.getCallInfo, value.getCachedCallInfo) ||
      !Objects.equals(value.getType, value.getCachedType)
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
        case _ =>
          Api.ExpressionUpdate.Payload.Value()
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
                payload
              )
            )
          )
        )
      )
    }
  }

  private def fireVisualisationUpdates(
    contextId: ContextId,
    value: ExpressionValue
  )(implicit ctx: RuntimeContext): Unit = {
    val visualisations =
      ctx.contextManager.findVisualisationForExpression(
        contextId,
        value.getExpressionId
      )
    visualisations foreach { visualisation =>
      emitVisualisationUpdate(
        contextId,
        visualisation,
        value.getExpressionId,
        value.getValue
      )
    }
  }

  /** Compute the visualisation of the expression value and send an update.
    *
    * @param contextId an identifier of an execution context
    * @param visualisation the visualisation data
    * @param expressionId the id of expression to visualise
    * @param expressionValue the value of expression to visualise
    * @param ctx the runtime context
    */
  def emitVisualisationUpdate(
    contextId: ContextId,
    visualisation: Visualisation,
    expressionId: UUID,
    expressionValue: AnyRef
  )(implicit ctx: RuntimeContext): Unit = {
    val errorMsgOrVisualisationData =
      Either
        .catchNonFatal {
          ctx.executionService.getLogger.log(
            Level.FINEST,
            s"Executing visualisation ${visualisation.expressionId}"
          )
          ctx.executionService.callFunction(
            visualisation.callback,
            expressionValue
          )
        }
        .leftMap(_.getMessage)
        .flatMap {
          case text: String =>
            Right(text.getBytes("UTF-8"))
          case text: Text =>
            Right(text.toString.getBytes("UTF-8"))
          case bytes: Array[Byte] =>
            Right(bytes)
          case other =>
            Left(s"Cannot encode ${other.getClass} to byte array")
        }

    errorMsgOrVisualisationData match {
      case Left(msg) =>
        ctx.endpoint.sendToClient(
          Api.Response(Api.VisualisationEvaluationFailed(contextId, msg))
        )

      case Right(data) =>
        ctx.executionService.getLogger.log(
          Level.FINEST,
          s"Sending visualisation ${visualisation.expressionId}"
        )
        ctx.endpoint.sendToClient(
          Api.Response(
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                visualisation.id,
                contextId,
                expressionId
              ),
              data
            )
          )
        )
    }
  }

  private def toMethodPointer(
    value: ExpressionValue
  ): Option[Api.MethodPointer] =
    for {
      call       <- Option(value.getCallInfo)
      moduleName <- Option(call.getModuleName)
      typeName   <- Option(call.getTypeName)
    } yield Api.MethodPointer(
      moduleName.toString,
      typeName.toString,
      call.getFunctionName
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

}

object ProgramExecutionSupport {

  /** An execution frame.
    *
    * @param item the executionitem
    * @param cache the cache of this stack frame
    */
  sealed private case class ExecutionFrame(
    item: ExecutionItem,
    cache: RuntimeCache
  )

  /** A local call frame defined by the expression id.
    *
    * @param expressionId the id of the expression
    * @param cache the cache of this frame
    */
  sealed private case class LocalCallFrame(
    expressionId: UUID,
    cache: RuntimeCache
  )
}
