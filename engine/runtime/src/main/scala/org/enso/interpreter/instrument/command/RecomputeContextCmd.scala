package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.{CacheInvalidation, InstrumentFrame}
import org.enso.interpreter.instrument.execution.{Executable, RuntimeContext}
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters._

/** A command that forces a recomputation of the current position.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class RecomputeContextCmd(
  maybeRequestId: Option[RequestId],
  request: Api.RecomputeContextRequest
) extends Command(maybeRequestId) {

  /** @inheritdoc */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    if (doesContextExist) {
      invalidateCache() flatMap (scheduleExecutionIfNeeded(_))
    } else {
      replyWithContextNotExistError()
    }

  private def invalidateCache()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Boolean] = {
    Future {
      ctx.jobControlPlane.abortJobs(request.contextId)
      val stack = ctx.contextManager.getStack(request.contextId)
      if (stack.isEmpty) {
        reply(Api.EmptyStackError(request.contextId))
        false
      } else {
        val cacheInvalidationCommands = request.expressions.toSeq
          .map(CacheInvalidation.Command(_))
          .map(CacheInvalidation(CacheInvalidation.StackSelector.Top, _))
        CacheInvalidation.runAll(
          stack,
          cacheInvalidationCommands
        )

        sendPendingUpdates(stack)
        reply(Api.RecomputeContextResponse(request.contextId))
        true
      }
    }
  }

  private def sendPendingUpdates(
    stack: Iterable[InstrumentFrame]
  )(implicit ctx: RuntimeContext): Unit = {
    val invalidatedExpressions =
      request.expressions
        .map {
          case expressions: Api.InvalidatedExpressions.Expressions =>
            expressions.value.toSet
          case _: Api.InvalidatedExpressions.All =>
            stack.headOption
              .map { frame =>
                frame.cache.getWeights.keySet().asScala.toSet
              }
              .getOrElse(Set())
        }
        .getOrElse(Set())
    if (invalidatedExpressions.nonEmpty) {
      val updates = invalidatedExpressions.map { expressionId =>
        Api.ExpressionUpdate(
          expressionId,
          None,
          None,
          Vector.empty,
          true,
          Api.ExpressionUpdate.Payload.Pending(None, None)
        )
      }
      ctx.endpoint.sendToClient(
        Api.Response(Api.ExpressionUpdates(request.contextId, updates))
      )
    }
  }

  private def doesContextExist(implicit ctx: RuntimeContext): Boolean = {
    ctx.contextManager.contains(request.contextId)
  }

  private def replyWithContextNotExistError()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    Future {
      reply(Api.ContextNotExistError(request.contextId))
    }

  private def scheduleExecutionIfNeeded(isStackNonEmpty: Boolean)(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    if (isStackNonEmpty) {
      val stack      = ctx.contextManager.getStack(request.contextId)
      val executable = Executable(request.contextId, stack)
      for {
        _ <- Future(ctx.jobProcessor.run(EnsureCompiledJob(executable.stack)))
        _ <- ctx.jobProcessor.run(new ExecuteJob(executable))
      } yield ()
    } else {
      Future.successful(())
    }
  }

}
