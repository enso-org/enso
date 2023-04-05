package org.enso.interpreter.instrument.command;

import java.util.UUID;
import org.enso.interpreter.instrument.CacheInvalidation;
import org.enso.interpreter.instrument.InstrumentFrame;
import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.interpreter.instrument.job.ExecuteJob;
import org.enso.polyglot.ExecutionEnvironment;
import scala.Option;
import scala.collection.mutable.Stack;
import scala.concurrent.ExecutionContext;
import scala.concurrent.Future;
import scala.runtime.BoxedUnit;

/** The command to set the runtime execution environment. */
public class SetExecutionEnvironmentCommand extends Command {

  private final UUID contextId;
  private final ExecutionEnvironment executionEnvironment;

  public SetExecutionEnvironmentCommand(
      Option<UUID> maybeRequestId, UUID contextId, ExecutionEnvironment executionEnvironment) {
    super(maybeRequestId);
    this.contextId = contextId;
    this.executionEnvironment = executionEnvironment;
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
      ExecutionEnvironment executionEnvironment, UUID contextId, RuntimeContext ctx) {
    ctx.locking().acquireWriteCompilationLock();
    try {
      ctx.jobControlPlane().abortJobs(contextId);
      ctx.executionService().getContext().setExecutionEnvironment(executionEnvironment);
      Stack<InstrumentFrame> stack = ctx.contextManager().getStack(contextId);
      CacheInvalidation.invalidateAll(stack);
      ctx.jobProcessor()
          .run(new ExecuteJob(contextId, stack.toList()));
    } finally {
      ctx.locking().releaseWriteCompilationLock();
    }
  }
}
