package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.InstrumentFrame
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

/**
  * A command that pops an item from a stack.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class PopContextCmd(
  maybeRequestId: Option[RequestId],
  request: Api.PopContextRequest
) extends Command
    with ProgramExecutionSupport {

  /** @inheritdoc **/
  override def execute(implicit ctx: RuntimeContext): Unit = {
    if (ctx.contextManager.get(request.contextId).isDefined) {
      val payload = ctx.contextManager.pop(request.contextId) match {
        case Some(InstrumentFrame(_: Api.StackItem.ExplicitCall, _)) =>
          Api.PopContextResponse(request.contextId)
        case Some(InstrumentFrame(_: Api.StackItem.LocalCall, _)) =>
          val stack = ctx.contextManager.getStack(request.contextId)
          withContext(runProgram(request.contextId, stack.toList)) match {
            case Right(()) => Api.PopContextResponse(request.contextId)
            case Left(e)   => Api.ExecutionFailed(request.contextId, e)
          }
        case None =>
          Api.EmptyStackError(request.contextId)
      }
      ctx.endpoint.sendToClient(Api.Response(maybeRequestId, payload))
    } else {
      ctx.endpoint.sendToClient(
        Api
          .Response(maybeRequestId, Api.ContextNotExistError(request.contextId))
      )
    }
  }

}
