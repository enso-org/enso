package org.enso.interpreter.instrument.command

import org.enso.compiler.pass.analyse.CachePreferenceAnalysis
import org.enso.interpreter.instrument.{CacheInvalidation, InstrumentFrame}
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.jdk.OptionConverters._

/**
  * A command that pushes an item onto a stack.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class PushContextCmd(
  maybeRequestId: Option[RequestId],
  request: Api.PushContextRequest
) extends Command
    with ProgramExecutionSupport {

  /** @inheritdoc **/
  override def execute(implicit ctx: RuntimeContext): Unit = {
    if (ctx.contextManager.get(request.contextId).isDefined) {
      val stack = ctx.contextManager.getStack(request.contextId)
      val payload = request.stackItem match {
        case _: Api.StackItem.ExplicitCall if stack.isEmpty =>
          ctx.contextManager.push(request.contextId, request.stackItem)
          getCacheMetadata(stack) match {
            case Some(metadata) =>
              CacheInvalidation.run(
                stack,
                CacheInvalidation(
                  CacheInvalidation.StackSelector.Top,
                  CacheInvalidation.Command.SetMetadata(metadata)
                )
              )
              withContext(runProgram(request.contextId, stack.toList)) match {
                case Right(()) => Api.PushContextResponse(request.contextId)
                case Left(e)   => Api.ExecutionFailed(request.contextId, e)
              }
            case None =>
              Api.InvalidStackItemError(request.contextId)
          }
        case _: Api.StackItem.LocalCall if stack.nonEmpty =>
          ctx.contextManager.push(request.contextId, request.stackItem)
          getCacheMetadata(stack) match {
            case Some(metadata) =>
              CacheInvalidation.run(
                stack,
                CacheInvalidation(
                  CacheInvalidation.StackSelector.Top,
                  CacheInvalidation.Command.SetMetadata(metadata)
                )
              )
              withContext(runProgram(request.contextId, stack.toList)) match {
                case Right(()) => Api.PushContextResponse(request.contextId)
                case Left(e)   => Api.ExecutionFailed(request.contextId, e)
              }
            case None =>
              Api.InvalidStackItemError(request.contextId)
          }
        case _ =>
          Api.InvalidStackItemError(request.contextId)
      }
      ctx.endpoint.sendToClient(Api.Response(maybeRequestId, payload))
    } else {
      ctx.endpoint.sendToClient(
        Api
          .Response(maybeRequestId, Api.ContextNotExistError(request.contextId))
      )
    }
  }

  private def getCacheMetadata(
    stack: Iterable[InstrumentFrame]
  )(implicit ctx: RuntimeContext): Option[CachePreferenceAnalysis.Metadata] =
    stack.lastOption flatMap {
      case InstrumentFrame(Api.StackItem.ExplicitCall(ptr, _, _), _) =>
        ctx.executionService.getContext.getModuleForFile(ptr.file).toScala.map {
          module =>
            module
              .parseIr(ctx.executionService.getContext)
              .unsafeGetMetadata(
                CachePreferenceAnalysis,
                "Empty cache preference metadata"
              )
        }
      case _ => None
    }
}
