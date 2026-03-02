package org.enso.interpreter.instrument.job;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import org.enso.interpreter.instrument.InstrumentFrame;
import org.enso.interpreter.instrument.execution.Executable;
import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.interpreter.runtime.state.ExecutionEnvironment;
import org.enso.polyglot.runtime.Runtime$Api$ExecutionComplete;
import org.enso.polyglot.runtime.Runtime$Api$ExecutionEnvironment;
import org.enso.polyglot.runtime.Runtime$Api$ExecutionFailed;
import org.enso.polyglot.runtime.Runtime$Api$ExecutionResult$Diagnostic;
import org.enso.polyglot.runtime.Runtime$Api$ExecutionResult$Failure;
import org.enso.polyglot.runtime.Runtime$Api$ExecutionUpdate;
import org.enso.polyglot.runtime.Runtime$Api$Response$;
import org.enso.scala.wrapper.ScalaConversions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.Option;

/**
 * A job responsible for executing a call stack for the provided context.
 *
 * @see Job
 * @see UniqueJob
 */
@SuppressWarnings("unchecked")
public class ExecuteJob extends Job<Void> implements UniqueJob<Void> {

  private static final Logger logger = LoggerFactory.getLogger(ExecuteJob.class);
  private static final String PENDING_VISUALIZATIONS_TRIGGER_CONTEXT = "pending visualizations";

  private final UUID contextId;
  private final scala.collection.immutable.List<InstrumentFrame> stack;
  private final Option<?> executionEnvironment;
  private final String triggerContext;
  private final scala.collection.Iterable<UUID> visualizationTriggered;

  private String threadName = "<unknown>";
  private volatile boolean hasStarted = false;
  private UUID jobId;

  /**
   * Create an ExecuteJob.
   *
   * @param contextId an identifier of a context to execute
   * @param stack a call stack to execute
   * @param executionEnvironment the execution environment to use
   * @param triggerContext human-readable explanation for execution job
   * @param visualizationTriggered the UUIDs of expressions that triggered this execution
   */
  public ExecuteJob(
      UUID contextId,
      scala.collection.immutable.List<InstrumentFrame> stack,
      Option<?> executionEnvironment,
      String triggerContext,
      scala.collection.Iterable<UUID> visualizationTriggered) {
    super(
        ScalaConversions.cons(contextId, ScalaConversions.nil()),
        computeIsCancellable(executionEnvironment),
        computeMayInterruptIfRunning(executionEnvironment));
    this.contextId = contextId;
    this.stack = stack;
    this.executionEnvironment = executionEnvironment;
    this.triggerContext = triggerContext;
    this.visualizationTriggered = visualizationTriggered;
  }

  /**
   * Create an ExecuteJob with no visualization triggered.
   *
   * @param contextId an identifier of a context to execute
   * @param stack a call stack to execute
   * @param executionEnvironment the execution environment to use
   * @param triggerContext human-readable explanation for execution job
   */
  public ExecuteJob(
      UUID contextId,
      scala.collection.immutable.List<InstrumentFrame> stack,
      Option<?> executionEnvironment,
      String triggerContext) {
    this(contextId, stack, executionEnvironment, triggerContext, ScalaConversions.nil());
  }

  /**
   * Computes the isCancellable flag based on the execution environment. Returns true if the
   * environment is not Live (or is not set).
   */
  private static boolean computeIsCancellable(Option<?> executionEnvironment) {
    if (executionEnvironment.isEmpty()) {
      return true;
    }
    return !((Runtime$Api$ExecutionEnvironment) executionEnvironment.get()).name().equals("live");
  }

  /**
   * Computes the mayInterruptIfRunning flag based on the execution environment. Returns true if the
   * environment is not Live (or is not set).
   */
  private static boolean computeMayInterruptIfRunning(Option<?> executionEnvironment) {
    if (executionEnvironment.isEmpty()) {
      return true;
    }
    return !((Runtime$Api$ExecutionEnvironment) executionEnvironment.get()).name().equals("live");
  }

  public UUID contextId() {
    return contextId;
  }

  public scala.collection.immutable.List<InstrumentFrame> stack() {
    return stack;
  }

  public Option<?> executionEnvironment() {
    return executionEnvironment;
  }

  public scala.collection.Iterable<UUID> visualizationTriggered() {
    return visualizationTriggered;
  }

  @Override
  public String threadNameExecutingJob() {
    return threadName;
  }

  @Override
  public boolean hasStarted() {
    return hasStarted;
  }

  @Override
  public void setJobId(UUID id) {
    this.jobId = id;
  }

  @Override
  public boolean equalsTo(UniqueJob<?> that) {
    if (that instanceof ExecuteJob e) {
      return e.contextId.equals(contextId)
          && e.stack.map(InstrumentFrame::item).equals(stack.map(InstrumentFrame::item))
          && e.executionEnvironment.equals(executionEnvironment);
    }
    return false;
  }

  @Override
  public Void runImpl(RuntimeContext ctx) {
    hasStarted = true;
    threadName = Thread.currentThread().getName();
    try {
      logger.debug("Starting ExecuteJob[{}, trigger={}]", jobId, triggerContext);
      execute(ctx);
    } catch (Throwable t) {
      logger.error("Failed to execute", t);
      var errorMsg = extractErrorMessage(t);
      ctx.endpoint()
          .sendToClient(
              Runtime$Api$Response$.MODULE$.apply(
                  new Runtime$Api$ExecutionFailed(
                      contextId,
                      new Runtime$Api$ExecutionResult$Failure(errorMsg, scala.Option.empty()))));
    } finally {
      logger.trace("Finished ExecuteJob[{}]", jobId);
    }
    return null;
  }

  private static String extractErrorMessage(Throwable t) {
    if (t.getMessage() != null) {
      return t.getMessage();
    }
    if (t.getCause() != null) {
      var cause = t.getCause();
      if (cause.getMessage() != null) {
        return cause.getMessage();
      }
      return cause.getClass().getSimpleName();
    }
    return t.getClass().getSimpleName();
  }

  private void execute(RuntimeContext ctx) {
    ctx.state().executionHooks().run();

    ctx.locking()
        .withReadContextLock(
            ctx.locking().getOrCreateContextLock(contextId),
            this.getClass(),
            () -> {
              ctx.locking()
                  .withReadCompilationLock(
                      this.getClass(),
                      () -> {
                        try {
                          ExecutionEnvironment originalExecutionEnvironment = null;
                          if (executionEnvironment.isDefined()) {
                            var env = (Runtime$Api$ExecutionEnvironment) executionEnvironment.get();
                            originalExecutionEnvironment =
                                ctx.executionService()
                                    .setExecutionInstrument(
                                        ExecutionEnvironment.forName(env.name()))
                                    .toCompletableFuture()
                                    .get();
                          }
                          Option<?> outcome;
                          try {
                            outcome =
                                ProgramExecutionSupport$.MODULE$.runProgram(contextId, stack, ctx);
                          } finally {
                            if (originalExecutionEnvironment != null) {
                              ctx.executionService()
                                  .setExecutionInstrument(originalExecutionEnvironment)
                                  .toCompletableFuture()
                                  .get();
                            }
                          }
                          handleOutcome(outcome, ctx);
                        } catch (ExecutionException e) {
                          ctx.endpoint()
                              .sendToClient(
                                  Runtime$Api$Response$.MODULE$.apply(
                                      new Runtime$Api$ExecutionFailed(
                                          contextId,
                                          new Runtime$Api$ExecutionResult$Failure(
                                              e.getMessage(), scala.Option.empty()))));
                          throw e;
                        }
                        rescheduleIfNeeded(ctx);
                        return null;
                      });
              return null;
            });
  }

  private void handleOutcome(Option<?> outcome, RuntimeContext ctx) {
    if (outcome.isDefined()) {
      var result = outcome.get();
      if (result instanceof Runtime$Api$ExecutionResult$Diagnostic diagnostic) {
        if (diagnostic.isError()) {
          ctx.endpoint()
              .sendToClient(
                  Runtime$Api$Response$.MODULE$.apply(
                      new Runtime$Api$ExecutionFailed(contextId, diagnostic)));
        } else {
          ctx.endpoint()
              .sendToClient(
                  Runtime$Api$Response$.MODULE$.apply(
                      new Runtime$Api$ExecutionUpdate(
                          contextId, ScalaConversions.seq(List.of(diagnostic)))));
          ctx.endpoint()
              .sendToClient(
                  Runtime$Api$Response$.MODULE$.apply(
                      new Runtime$Api$ExecutionComplete(contextId)));
        }
      } else if (result instanceof Runtime$Api$ExecutionResult$Failure failure) {
        ctx.endpoint()
            .sendToClient(
                Runtime$Api$Response$.MODULE$.apply(
                    new Runtime$Api$ExecutionFailed(contextId, failure)));
      }
    } else {
      ctx.endpoint()
          .sendToClient(
              Runtime$Api$Response$.MODULE$.apply(new Runtime$Api$ExecutionComplete(contextId)));
    }
  }

  private void rescheduleIfNeeded(RuntimeContext ctx) {
    if (!mayInterruptIfRunning()) {
      return;
    }
    var holder = ctx.contextManager().getVisualizationHolder(contextId);
    var unevaluatedVisualizations = holder.getAllUnevaluated();
    var unevaluatedVisualizationIds = unevaluatedVisualizations.toList().map(v -> v.id());
    if (unevaluatedVisualizationIds.nonEmpty()) {
      if (unevaluatedVisualizationIds.equals(visualizationTriggered)
          || PENDING_VISUALIZATIONS_TRIGGER_CONTEXT.equals(triggerContext)) {
        // This is a retry that made no progress
        return;
      }
      logger.trace(
          "Rescheduling ExecuteJob[{}] to process pending visualizations {}",
          jobId,
          unevaluatedVisualizationIds);
      ctx.jobProcessor()
          .run(
              new ExecuteJob(
                  contextId,
                  stack,
                  executionEnvironment,
                  PENDING_VISUALIZATIONS_TRIGGER_CONTEXT,
                  unevaluatedVisualizationIds));
    }
  }

  @Override
  public String toString() {
    return "ExecuteJob(contextId="
        + contextId
        + ", jobId="
        + jobId
        + ", triggeredByVisualization="
        + visualizationTriggered
        + ")";
  }

  /**
   * Create execute job from the executable.
   *
   * @param executable the executable to run
   * @param triggerContext human-readable explanation for execution job
   * @param visualizationTriggered the UUID of an expression that triggered this execution, empty
   *     otherwise
   * @return the new execute job
   */
  public static ExecuteJob apply(
      Executable executable, String triggerContext, Option<UUID> visualizationTriggered) {
    return new ExecuteJob(
        executable.contextId(),
        executable.stack().toList(),
        scala.Option.empty(),
        triggerContext,
        visualizationTriggered.toList());
  }

  /**
   * Create execute job from the executable.
   *
   * @param executable the executable to run
   * @param triggerContext human-readable explanation for execution job
   * @return the new execute job
   */
  public static ExecuteJob apply(Executable executable, String triggerContext) {
    return apply(executable, triggerContext, scala.Option.empty());
  }

  /**
   * Create execute job from the context and stack.
   *
   * @param contextId the contextId to execute
   * @param stack the stack to execute
   * @param triggerContext human-readable explanation for execution job
   * @return new execute job
   */
  public static ExecuteJob apply(
      UUID contextId,
      scala.collection.immutable.List<InstrumentFrame> stack,
      String triggerContext) {
    return new ExecuteJob(contextId, stack, scala.Option.empty(), triggerContext);
  }
}
