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

  @SuppressWarnings("unchecked")
  private void setExecutionEnvironment(
      Runtime$Api$ExecutionEnvironment executionEnvironment, UUID contextId, RuntimeContext ctx) {
    var logger = ctx.executionService().getLogger();
    ctx.locking()
        .withContextLock(
            ctx.locking().getOrCreateContextLock(contextId),
            this.getClass(),
            () -> {
              var oldEnvironmentName =
                  ctx.executionService().getContext().getGlobalExecutionEnvironment().getName();
              if (!oldEnvironmentName.equals(executionEnvironment.name())) {
                ctx.jobControlPlane()
                    .abortJobs(
                        contextId, "set execution environment to " + executionEnvironment.name());
                ctx.locking()
                    .withWriteCompilationLock(
                        this.getClass(),
                        () -> {
                          Stack<InstrumentFrame> stack = ctx.contextManager().getStack(contextId);
                          ctx.state()
                              .executionHooks()
                              .add(
                                  () ->
                                      ctx.locking()
                                          .withWriteCompilationLock(
                                              this.getClass(),
                                              () -> {
                                                ctx.executionService()
                                                    .getContext()
                                                    .setExecutionEnvironment(
                                                        ExecutionEnvironment.forName(
                                                            executionEnvironment.name()));
                                                return null;
                                              }));
                          CacheInvalidation.invalidateAll(stack);
                          ctx.jobProcessor().run(ExecuteJob.apply(contextId, stack.toList()));
                          reply(new Runtime$Api$SetExecutionEnvironmentResponse(contextId), ctx);
                          return null;
                        });
              } else {
                logger.log(
                    Level.FINE,
                    "Requested environment '{}' is the same as the current one. Request has no"
                        + " effect",
                    oldEnvironmentName);
                reply(new Runtime$Api$SetExecutionEnvironmentResponse(contextId), ctx);
              }
              return null;
            });
  }
}
