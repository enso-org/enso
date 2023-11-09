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
public class SetExecutionEnvironmentCommand extends AsynchronousCommand {

  private final UUID contextId;
  private final Runtime$Api$ExecutionEnvironment executionEnvironment;

  public SetExecutionEnvironmentCommand(
      Option<UUID> maybeRequestId, UUID contextId, Object executionEnvironment) {
    super(maybeRequestId);
    this.contextId = contextId;
    this.executionEnvironment = (Runtime$Api$ExecutionEnvironment) executionEnvironment;
  }

  @Override
  public Future<BoxedUnit> executeAsynchronously(RuntimeContext ctx, ExecutionContext ec) {
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
    long contextLockTimestamp = 0;
    long writeLockTimestamp = 0;

    try {
      contextLockTimestamp = ctx.locking().acquireContextLock(contextId);
      writeLockTimestamp = ctx.locking().acquireWriteCompilationLock();
      Stack<InstrumentFrame> stack = ctx.contextManager().getStack(contextId);
      ctx.jobControlPlane().abortJobs(contextId);
      ctx.executionService()
          .getContext()
          .setExecutionEnvironment(ExecutionEnvironment.forName(executionEnvironment.name()));
      CacheInvalidation.invalidateAll(stack);
      ctx.jobProcessor().run(ExecuteJob.apply(contextId, stack.toList()));
      reply(new Runtime$Api$SetExecutionEnvironmentResponse(contextId), ctx);
    } catch (InterruptedException ie) {
      logger.log(Level.WARNING, "Failed to acquire lock: interrupted", ie);
    } finally {
      if (writeLockTimestamp != 0) {
        ctx.locking().releaseWriteCompilationLock();
        logger.log(
            Level.FINEST,
            "Kept write compilation lock [{0}] for {1} milliseconds",
            new Object[] {
              this.getClass().getSimpleName(), System.currentTimeMillis() - writeLockTimestamp
            });
      }
      if (contextLockTimestamp != 0) {
        ctx.locking().releaseContextLock(contextId);
        logger.log(
            Level.FINEST,
            "Kept context lock [{0}] for {1} milliseconds",
            new Object[] {
              this.getClass().getSimpleName(), System.currentTimeMillis() - contextLockTimestamp
            });
      }
    }
  }
}
