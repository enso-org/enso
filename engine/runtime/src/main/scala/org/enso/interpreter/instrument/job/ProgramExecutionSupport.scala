package org.enso.interpreter.instrument.job

import java.util.{Objects, UUID}
import java.util.function.Consumer
import java.util.logging.Level

import cats.implicits._
import org.enso.interpreter.instrument.IdExecutionInstrument.{
  ExpressionCall,
  ExpressionValue
}
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.ProgramExecutionSupport.{
  ExecutionFrame,
  ExecutionItem,
  LocalCallFrame
}
import org.enso.interpreter.instrument.{
  InstrumentFrame,
  RuntimeCache,
  Visualisation
}
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode.FunctionCall
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.ContextId

/**
  * Provides support for executing Enso code. Adds convenient methods to
  * run Enso programs in a Truffle context.
  */
trait ProgramExecutionSupport {

  /**
    * Runs an Enso program.
    *
    * @param executionFrame an execution frame
    * @param callStack a call stack
    * @param onComputedCallback a listener of computed values
    * @param onCachedCallback a listener of cached values
    */
  @scala.annotation.tailrec
  final private def runProgram(
    executionFrame: ExecutionFrame,
    callStack: List[LocalCallFrame],
    onComputedCallback: Consumer[ExpressionValue],
    onCachedCallback: Consumer[ExpressionValue]
  )(implicit ctx: RuntimeContext): Unit = {
    var enterables: Map[UUID, FunctionCall] = Map()
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
          module,
          cons,
          function,
          cache,
          callStack.headOption.map(_.expressionId).orNull,
          computedCallback,
          onCachedCallback,
          callablesCallback
        )
      case ExecutionFrame(ExecutionItem.CallData(callData), cache) =>
        ctx.executionService.execute(
          callData,
          cache,
          callStack.headOption.map(_.expressionId).orNull,
          computedCallback,
          onCachedCallback,
          callablesCallback
        )
    }

    callStack match {
      case Nil => ()
      case item :: tail =>
        enterables.get(item.expressionId) match {
          case Some(call) =>
            runProgram(
              ExecutionFrame(ExecutionItem.CallData(call), item.cache),
              tail,
              onComputedCallback,
              onCachedCallback
            )
          case None =>
            ()
        }
    }
  }

  /**
    * Runs an Enso program.
    *
    * @param contextId an identifier of an execution context
    * @param stack a call stack
    * @param updatedVisualisations a list of updated visualisations
    * @param ctx a runtime context
    * @return either an error message or Unit signaling completion of a program
    */
  final def runProgram(
    contextId: Api.ContextId,
    stack: List[InstrumentFrame],
    updatedVisualisations: Seq[Api.ExpressionId]
  )(implicit ctx: RuntimeContext): Either[String, Unit] = {
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
    def getName(item: ExecutionItem): String =
      item match {
        case ExecutionItem.Method(_, _, function) => function
        case ExecutionItem.CallData(call)         => call.getFunction.getName
      }

    val onCachedValueCallback: Consumer[ExpressionValue] = { value =>
      if (updatedVisualisations.contains(value.getExpressionId)) {
        ctx.executionService.getLogger.finer(s"ON_CACHED $value")
        fireVisualisationUpdates(contextId, value)
      }
    }

    val onComputedValueCallback: Consumer[ExpressionValue] = { value =>
      ctx.executionService.getLogger.finer(s"ON_COMPUTED $value")
      sendValueUpdate(contextId, value)
      fireVisualisationUpdates(contextId, value)
    }

    val (explicitCallOpt, localCalls) = unwind(stack, Nil, Nil)
    for {
      stackItem <- Either.fromOption(explicitCallOpt, "stack is empty")
      _ <-
        Either
          .catchNonFatal(
            runProgram(
              stackItem,
              localCalls,
              onComputedValueCallback,
              onCachedValueCallback
            )
          )
          .leftMap { ex =>
            ctx.executionService.getLogger.log(
              Level.FINE,
              s"Error executing a function '${getName(stackItem.item)}'",
              ex
            )
            s"error in function: ${getName(stackItem.item)}"
          }
    } yield ()
  }

  private def sendValueUpdate(
    contextId: ContextId,
    value: ExpressionValue
  )(implicit ctx: RuntimeContext): Unit = {
    if (
      !Objects.equals(value.getType, value.getCachedType) ||
      !Objects.equals(value.getCallInfo, value.getCachedCallInfo)
    ) {
      ctx.endpoint.sendToClient(
        Api.Response(
          Api.ExpressionValuesComputed(
            contextId,
            Vector(
              Api.ExpressionValueUpdate(
                value.getExpressionId,
                Some(value.getType),
                toMethodPointer(value)
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
      emitVisualisationUpdate(contextId, value, visualisation)
    }
  }

  private def emitVisualisationUpdate(
    contextId: ContextId,
    value: ExpressionValue,
    visualisation: Visualisation
  )(implicit ctx: RuntimeContext): Unit = {
    val errorMsgOrVisualisationData =
      Either
        .catchNonFatal {
          ctx.executionService.callFunction(
            visualisation.callback,
            value.getValue
          )
        }
        .leftMap(_.getMessage)
        .flatMap {
          case text: String       => Right(text.getBytes("UTF-8"))
          case bytes: Array[Byte] => Right(bytes)
          case other =>
            Left(s"Cannot encode ${other.getClass} to byte array")
        }

    errorMsgOrVisualisationData match {
      case Left(msg) =>
        ctx.endpoint.sendToClient(
          Api.Response(Api.VisualisationEvaluationFailed(contextId, msg))
        )

      case Right(data) =>
        ctx.endpoint.sendToClient(
          Api.Response(
            Api.VisualisationUpdate(
              Api.VisualisationContext(
                visualisation.id,
                contextId,
                value.getExpressionId
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
      typeName   <- Option(call.getTypeName).map(_.item)
    } yield Api.MethodPointer(
      moduleName.toString,
      typeName,
      call.getFunctionName
    )
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

  /** An execution item. */
  sealed private trait ExecutionItem
  private object ExecutionItem {

    /** The explicit method call.
      *
      * @param module the module containing the method
      * @param constructor the type on which the method is defined
      * @param function the method name
      */
    case class Method(module: String, constructor: String, function: String)
        extends ExecutionItem

    object Method {

      /** Construct the method call from the [[Api.StackItem.ExplicitCall]].
        *
        * @param call the Api call
        * @return the method call
        */
      def apply(call: Api.StackItem.ExplicitCall): Method =
        Method(
          call.methodPointer.module,
          call.methodPointer.definedOnType,
          call.methodPointer.name
        )
    }

    /** The call data captured during the program execution.
      *
      * @param callData the fucntion call data
      */
    case class CallData(callData: FunctionCall) extends ExecutionItem
  }
}
