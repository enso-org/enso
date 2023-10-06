package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.ContextId

import scala.concurrent.{ExecutionContext, Future}

abstract class ContextCmd(
  contextId: ContextId,
  maybeRequestId: Option[Api.RequestId]
) extends AsynchronousCommand(maybeRequestId) {

  protected def executeCmd()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit]

  override def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    if (ctx.contextManager.contains(contextId)) {
      executeCmd()
    } else {
      replyWithContextNotExistError()
    }
  }

  protected def replyWithContextNotExistError()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    Future {
      reply(
        Api.ContextNotExistError(contextId)
      )
    }
  }

}
