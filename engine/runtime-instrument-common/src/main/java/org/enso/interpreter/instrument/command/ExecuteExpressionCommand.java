package org.enso.interpreter.instrument.command;

import java.util.UUID;
import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.interpreter.instrument.job.ExecuteExpressionJob;
import org.enso.interpreter.instrument.job.ExecuteJob;
import org.enso.polyglot.runtime.Runtime$Api$VisualizationAttached;
import scala.Option;
import scala.concurrent.ExecutionContext;
import scala.concurrent.Future;
import scala.runtime.BoxedUnit;

/** The command that handles the execute expression request. */
public final class ExecuteExpressionCommand extends ContextCmd {

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
    super(contextId, maybeRequestId);
    this.contextId = contextId;
    this.visualizationId = visualizationId;
    this.expressionId = expressionId;
    this.expression = expression;
  }

  @Override
  public Future<BoxedUnit> executeCmd(RuntimeContext ctx, ExecutionContext ec) {
    reply(new Runtime$Api$VisualizationAttached(), ctx);
    return ctx.jobProcessor()
        .run(new ExecuteExpressionJob(contextId, visualizationId, expressionId, expression))
        .flatMap(executable -> ctx.jobProcessor().run(ExecuteJob.apply(executable)), ec);
  }
}
