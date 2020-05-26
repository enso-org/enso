package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

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
        case call: Api.StackItem.ExplicitCall if stack.isEmpty =>
          ctx.contextManager.push(request.contextId, request.stackItem)
          withContext(runProgram(request.contextId, List(call))) match {
            case Right(()) => Api.PushContextResponse(request.contextId)
            case Left(e)   => Api.ExecutionFailed(request.contextId, e)
          }
        case _: Api.StackItem.LocalCall if stack.nonEmpty =>
          ctx.contextManager.push(request.contextId, request.stackItem)
          withContext(runProgram(request.contextId, stack.toList)) match {
            case Right(()) => Api.PushContextResponse(request.contextId)
            case Left(e)   => Api.ExecutionFailed(request.contextId, e)
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

}
