package org.enso.interpreter.instrument.command

import java.util

import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.pass.analyse.DataflowAnalysis
import org.enso.compiler.refactoring.IRUtils
import org.enso.interpreter.instrument.command.RecomputeContextCmd.InvalidateExpressions
import org.enso.interpreter.instrument.{
  CacheInvalidation,
  ExecutionConfig,
  InstrumentFrame,
  RefInvalidation
}
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.annotation.unused
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.IterableHasAsJava

/** A command that forces a recomputation of the current position.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class RecomputeContextCmd(
  maybeRequestId: Option[RequestId],
  request: Api.RecomputeContextRequest
) extends AsynchronousCommand(maybeRequestId) {

  /** @inheritdoc */
  override def executeAsynchronously(implicit
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
      ctx.locking.withReadContextLock(
        ctx.locking.getOrCreateContextLock(request.contextId),
        this.getClass,
        () => {
          ctx.executionService.getContext
            .getResourceManager()
            .scheduleFinalizationOfSystemReferences()
          ctx.jobControlPlane.abortJobs(
            request.contextId,
            "recompute context",
            false
          )
          val stack = ctx.contextManager.getStack(request.contextId)
          if (stack.isEmpty) {
            reply(Api.EmptyStackError(request.contextId))
            false
          } else {
            ctx.state.executionHooks.add(
              InvalidateExpressions(
                request.contextId,
                request.expressions,
                request.expressionConfigs
              )
            )
            reply(Api.RecomputeContextResponse(request.contextId))
            true
          }
        }
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
      val stack = ctx.contextManager.getStack(request.contextId)
      val executionConfig =
        ExecutionConfig.create(
          request.executionEnvironment,
          request.expressionConfigs
        )
      ctx.state.expressionExecutionState
        .setExpressionConfigs(executionConfig.expressionConfigs)
      for {
        _ <- ctx.jobProcessor.run(EnsureCompiledJob(stack))
        _ <- ctx.jobProcessor.run(
          ExecuteJob(
            request.contextId,
            stack.toList,
            request.executionEnvironment,
            "recompute context"
          )
        )
      } yield ()
    } else {
      Future.successful(())
    }
  }

}

object RecomputeContextCmd {

  /** Invalidate caches for the request. */
  sealed private case class InvalidateExpressions(
    contextId: Api.ContextId,
    expressions: Option[Api.InvalidatedExpressions],
    expressionConfigs: Seq[Api.ExpressionConfig]
  )(implicit ctx: RuntimeContext)
      extends Runnable {

    override def run(): Unit = {
      val stack = ctx.contextManager.getStack(contextId)

      val invalidationCommands = {
        val expressionsInvalidationCommands = expressions.toSeq
          .map(CacheInvalidation.Command(_))
          .map(CacheInvalidation(CacheInvalidation.StackSelector.All, _))
        val expressionConfigsDependentInvalidationCommands = {
          val expressionsToInvalidate = expressionConfigs
            .map(_.expressionId)
            .toVector
            .distinct

          if (expressionsToInvalidate.isEmpty) Seq.empty
          else {
            val cmd = CacheInvalidation.Command(
              Api.InvalidatedExpressions
                .Expressions(expressionsToInvalidate, "recompute")
            )
            Seq(CacheInvalidation(CacheInvalidation.StackSelector.All, cmd))
          }
        }
        val allInvalidationCommands =
          expressionsInvalidationCommands ++ expressionConfigsDependentInvalidationCommands
        allInvalidationCommands
      }

      ctx.locking.withWriteCompilationLock(
        classOf[RecomputeContextCmd],
        () => {
          sendPendingUpdates(stack, contextId, invalidationCommands)
          CacheInvalidation.runAll(stack, invalidationCommands)
        }
      )
    }
  }

  /** Invalidate dependent nodes of the provided expression.
    *
    * @param expressionId the expression id
    * @return commands to invalidate dependent nodes of the provided expression
    */
  @unused
  private def invalidateDependent(
    expressionId: Api.ExpressionId
  )(implicit ctx: RuntimeContext): Seq[CacheInvalidation] = {
    val builder = Vector.newBuilder[CacheInvalidation]
    ctx.executionService.getContext
      .findModuleByExpressionId(expressionId)
      .ifPresent { module =>
        module.getIr
          .getMetadata(DataflowAnalysis)
          .foreach { metadata =>
            val dependents =
              IRUtils
                .findByExternalId(module.getIr, expressionId)
                .map { ir =>
                  DataflowAnalysis.DependencyInfo.Type
                    .Static(ir.getId, ir.getExternalId)
                }
                .flatMap { expressionKey =>
                  metadata.dependents.getExternal(expressionKey)
                }
                .fold(Set(expressionId))(_ + expressionId)
            builder += CacheInvalidation(
              CacheInvalidation.StackSelector.All,
              CacheInvalidation.Command.InvalidateKeys(
                dependents,
                "invalidate dependends of " + expressionId
              )
            )
          }
      }

    builder.result()
  }

  private def sendPendingUpdates(
    stack: Iterable[InstrumentFrame],
    contextId: Api.ContextId,
    cacheInvalidations: Seq[CacheInvalidation]
  )(implicit ctx: RuntimeContext): Unit = {
    val builder         = Set.newBuilder[Api.ExpressionId]
    val runtimeAnalysis = ctx.state.runtimeAnalysis
    cacheInvalidations.map(_.command).foreach {
      case CacheInvalidation.Command.InvalidateAll =>
        stack
          .foreach { frame =>
            val toInvalidate = frame.cache.clear()
            toInvalidate.forEach { runtimeID =>
              runtimeAnalysis
                .get(runtimeID)
                .reset()
              builder.addOne(runtimeID.uuid())
            }
            frame.syncState.clearVisualizationSync()
          }
      case CacheInvalidation.Command.InvalidateKeys(expressionIds, _) =>
        val stackJ = new util.Stack[InstrumentFrame]
        stack.toList.reverse.foreach(stackJ.push)
        RefInvalidation
          .invalidateAffectedIDs(
            expressionIds.asJava,
            stackJ,
            ctx.contextManager.getVisualizationHolder(contextId),
            ctx.state.runtimeAnalysis
          )
          .stream()
          .forEach { id =>
            builder += id.uuid()
          }
      case _ =>
    }

    val invalidatedExpressions = builder.result()
    if (invalidatedExpressions.nonEmpty) {
      val updates = invalidatedExpressions.collect {
        case expressionId if expressionId ne null =>
          Api.ExpressionUpdate(
            expressionId,
            None,
            None,
            Vector.empty,
            true,
            false,
            Api.ExpressionUpdate.Payload.Pending(None, None)
          )
      }
      ctx.endpoint.sendToClient(
        Api.Response(Api.ExpressionUpdates(contextId, updates))
      )
    }
  }
}
