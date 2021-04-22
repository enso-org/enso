package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.InstrumentFrame
import org.enso.interpreter.instrument.execution.{Executable, RuntimeContext}
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}

/** A command that pops an item from a stack.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class PopContextCmd(
  maybeRequestId: Option[RequestId],
  request: Api.PopContextRequest
) extends Command(maybeRequestId) {

  /** @inheritdoc */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    if (doesContextExist) {
      popItemFromStack() flatMap { _ => scheduleExecutionIfNeeded() }
    } else {
      replyWithContextNotExistError()
    }

  private def replyWithContextNotExistError()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    Future {
      reply(Api.ContextNotExistError(request.contextId))
    }
  }

  private def popItemFromStack()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    Future {
      ctx.jobControlPlane.abortJobs(request.contextId)
      val maybeTopItem = ctx.contextManager.pop(request.contextId)
      if (maybeTopItem.isDefined) {
        reply(Api.PopContextResponse(request.contextId))
      } else {
        reply(Api.EmptyStackError(request.contextId))
      }
    }

  private def doesContextExist(implicit ctx: RuntimeContext): Boolean = {
    ctx.contextManager.contains(request.contextId)
  }

  private def scheduleExecutionIfNeeded()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    val stack = ctx.contextManager.getStack(request.contextId)
    if (stack.nonEmpty) {
      val executable = Executable(request.contextId, stack)
      for {
        _ <- Future(requireMethodPointersSynchronization(stack))
        _ <- Future(ctx.jobProcessor.run(EnsureCompiledJob(executable.stack)))
        _ <- ctx.jobProcessor.run(new ExecuteJob(executable))
      } yield ()
    } else {
      Future.successful(())
    }
  }

  private def requireMethodPointersSynchronization(
    stack: Iterable[InstrumentFrame]
  ): Unit = {
    stack.foreach(_.syncState.clearMethodPointersState())
  }

}
