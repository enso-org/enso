package org.enso.interpreter.instrument.command;

import java.util.UUID;
import java.util.logging.Level;
import org.enso.interpreter.instrument.CacheInvalidation;
import org.enso.interpreter.instrument.InstrumentFrame;
import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.interpreter.instrument.job.ExecuteJob;
import org.enso.interpreter.runtime.state.ExecutionEnvironment;
import org.enso.polyglot.runtime.Runtime$Api$ExecutionEnvironment;
import org.enso.polyglot.runtime.Runtime$Api$SetExecutionEnvironmentResponse;
import scala.Option;
import scala.collection.mutable.Stack;
import scala.concurrent.ExecutionContext;
import scala.concurrent.Future;
import scala.runtime.BoxedUnit;

/** The command to set the runtime execution environment. */
public class SetExecutionEnvironmentCommand extends Command {

  private final UUID contextId;
  private final Runtime$Api$ExecutionEnvironment executionEnvironment;

  public SetExecutionEnvironmentCommand(
      Option<UUID> maybeRequestId, UUID contextId, Object executionEnvironment) {
    super(maybeRequestId);
    this.contextId = contextId;
    this.executionEnvironment = (Runtime$Api$ExecutionEnvironment) executionEnvironment;
  }

  @Override
  public Future<BoxedUnit> execute(RuntimeContext ctx, ExecutionContext ec) {

    return Future.apply(
        () -> {
          setExecutionEnvironment(executionEnvironment, contextId, ctx);
          return BoxedUnit.UNIT;
        },
        ec);
  }

  private void setExecutionEnvironment(
      Runtime$Api$ExecutionEnvironment executionEnvironment, UUID contextId, RuntimeContext ctx) {
    var logger = ctx.executionService().getLogger();
    var contextLockTimestamp = ctx.locking().acquireContextLock(contextId);
    var writeLockTimestamp = ctx.locking().acquireWriteCompilationLock();

    try {
      Stack<InstrumentFrame> stack = ctx.contextManager().getStack(contextId);
      ctx.jobControlPlane().abortJobs(contextId);
      ctx.executionService()
          .getContext()
          .setExecutionEnvironment(ExecutionEnvironment.forName(executionEnvironment.name()));
      CacheInvalidation.invalidateAll(stack);
      ctx.jobProcessor().run(ExecuteJob.apply(contextId, stack.toList()));
      reply(new Runtime$Api$SetExecutionEnvironmentResponse(contextId), ctx);
    } finally {
      ctx.locking().releaseWriteCompilationLock();
      logger.log(
          Level.FINEST,
          "Kept write compilation lock [SetExecutionEnvironmentCommand] for "
              + (System.currentTimeMillis() - writeLockTimestamp)
              + " milliseconds");
      ctx.locking().releaseContextLock(contextId);
      logger.log(
          Level.FINEST,
          "Kept context lock [SetExecutionEnvironmentCommand] for "
              + (System.currentTimeMillis() - contextLockTimestamp)
              + " milliseconds");
    }
  }
}
