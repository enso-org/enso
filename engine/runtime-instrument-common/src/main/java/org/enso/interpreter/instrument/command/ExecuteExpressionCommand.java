package org.enso.interpreter.instrument.command;

import java.util.UUID;
import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.interpreter.instrument.job.ExecuteExpressionJob;
import org.enso.interpreter.instrument.job.ExecuteJob;
import org.enso.polyglot.runtime.Runtime$Api$ContextNotExistError;
import org.enso.polyglot.runtime.Runtime$Api$VisualizationAttached;
import scala.Option;
import scala.concurrent.ExecutionContext;

/** The command that handles the execute expression request. */
public final class ExecuteExpressionCommand extends SynchronousCommand {

  private final UUID contextId;
  private final UUID visualizationId;
  private final UUID expressionId;
  private final String expression;

  /**
   * Create the {@link ExecuteExpressionCommand}.
   *
   * @param maybeRequestId the request id.
   * @param contextId the execution context id.
   * @param visualizationId the visualization id.
   * @param expressionId the expression providing the execution scope.
   * @param expression the expression to execute.
   */
  public ExecuteExpressionCommand(
      Option<UUID> maybeRequestId,
      UUID contextId,
      UUID visualizationId,
      UUID expressionId,
      String expression) {
    super(maybeRequestId);
    this.contextId = contextId;
    this.visualizationId = visualizationId;
    this.expressionId = expressionId;
    this.expression = expression;
  }

  @Override
  @SuppressWarnings("unchecked")
  public void executeSynchronously(RuntimeContext ctx, ExecutionContext ec) {
    if (ctx.contextManager().contains(contextId)) {
      reply(new Runtime$Api$VisualizationAttached(), ctx);
      ctx.jobControlPlane()
          .abortJobs(
              contextId,
              "execute expression for expression "
                  + expressionId
                  + " in visualization "
                  + visualizationId,
              job -> {
                if (job instanceof ExecuteJob e) {
                  return e.visualizationTriggered();
                } else {
                  return job instanceof ExecuteExpressionJob;
                }
              });
      ctx.jobProcessor()
          .run(new ExecuteExpressionJob(contextId, visualizationId, expressionId, expression))
          .flatMap(executable -> ctx.jobProcessor().run(ExecuteJob.apply(executable, true)), ec);
    } else {
      reply(new Runtime$Api$ContextNotExistError(contextId), ctx);
    }
  }
}
