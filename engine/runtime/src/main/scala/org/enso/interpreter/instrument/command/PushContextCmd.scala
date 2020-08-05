package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.{Executable, RuntimeContext}
import org.enso.interpreter.instrument.job.{EnsureCompiledStackJob, ExecuteJob}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}

/**
  * A command that pushes an item onto a stack.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class PushContextCmd(
  maybeRequestId: Option[RequestId],
  request: Api.PushContextRequest
) extends Command(maybeRequestId) {

  /** @inheritdoc */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    if (doesContextExist) {
      pushItemOntoStack() flatMap (scheduleExecutionIfNeeded(_))
    } else {
      replyWithContextNotExistError()
    }

  private def doesContextExist(implicit ctx: RuntimeContext): Boolean = {
    ctx.contextManager.contains(request.contextId)
  }

  private def replyWithContextNotExistError()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    Future {
      reply(Api.ContextNotExistError(request.contextId))
    }
  }

  private def pushItemOntoStack()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Boolean] =
    Future {
      ctx.jobControlPlane.abortJobs(request.contextId)
      val stack = ctx.contextManager.getStack(request.contextId)
      val pushed = request.stackItem match {
        case _: Api.StackItem.ExplicitCall if stack.isEmpty =>
          ctx.contextManager.push(request.contextId, request.stackItem)
          true

        case _: Api.StackItem.LocalCall if stack.nonEmpty =>
          ctx.contextManager.push(request.contextId, request.stackItem)
          true

        case _ =>
          false
      }
      if (pushed) {
        reply(Api.PushContextResponse(request.contextId))
      } else {
        reply(Api.InvalidStackItemError(request.contextId))
      }
      pushed
    }

  private def scheduleExecutionIfNeeded(pushed: Boolean)(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    if (pushed) {
      val stack = ctx.contextManager.getStack(request.contextId)
      val executable = Executable(
        request.contextId,
        stack,
        Seq(),
        sendMethodCallUpdates = false
      )
      for {
        _ <- ctx.jobProcessor.run(new EnsureCompiledStackJob(executable.stack))
        _ <- ctx.jobProcessor.run(new ExecuteJob(executable))
      } yield ()
    } else {
      Future.successful(())
    }
  }

}
