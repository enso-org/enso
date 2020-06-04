package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.CacheInvalidation
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

/**
  * A command that forces a recomputation of the current position.
  *
  * == Caching ==
  *
  * Invalidate top frame according to the [[Api.InvalidatedExpressions]]
  * parameter.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class RecomputeContextCmd(
  maybeRequestId: Option[RequestId],
  request: Api.RecomputeContextRequest
) extends Command
    with ProgramExecutionSupport {

  /** @inheritdoc **/
  override def execute(implicit ctx: RuntimeContext): Unit = {
    if (ctx.contextManager.get(request.contextId).isDefined) {
      val stack = ctx.contextManager.getStack(request.contextId)
      val payload = if (stack.isEmpty) {
        Api.EmptyStackError(request.contextId)
      } else {
        val cacheInvalidationCommands = request.expressions.toSeq
          .map(CacheInvalidation.Command(_))
          .map(CacheInvalidation(CacheInvalidation.StackSelector.Top, _))
        CacheInvalidation.runAll(stack, cacheInvalidationCommands)
        withContext(runProgram(request.contextId, stack.toList)) match {
          case Right(()) => Api.RecomputeContextResponse(request.contextId)
          case Left(e)   => Api.ExecutionFailed(request.contextId, e)
        }
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
