package org.enso.interpreter.instrument.job;

import com.oracle.truffle.api.TruffleLogger;
import java.util.UUID;
import java.util.logging.Level;
import org.enso.interpreter.instrument.Visualization;
import org.enso.interpreter.instrument.execution.Executable;
import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.interpreter.util.ScalaConversions;

/** The job that schedules the execution of the expression. */
public class ExecuteExpressionJob extends Job<Executable> {

  private final UUID contextId;
  private final UUID visualizationId;
  private final UUID expressionId;
  private final String expression;

  /**
   * Create the {@link ExecuteExpressionJob}.
   *
   * @param contextId the execution context id.
   * @param visualizationId the visualization id.
   * @param expressionId the expression providing the execution scope.
   * @param expression the expression to execute.
   */
  public ExecuteExpressionJob(
      UUID contextId, UUID visualizationId, UUID expressionId, String expression) {
    super(ScalaConversions.cons(contextId, ScalaConversions.nil()), false, false);
    this.contextId = contextId;
    this.visualizationId = visualizationId;
    this.expressionId = expressionId;
    this.expression = expression;
  }

  @Override
  public Executable run(RuntimeContext ctx) {
    TruffleLogger logger = ctx.executionService().getLogger();
    long lockTimestamp = ctx.locking().acquireContextLock(contextId);

    try {
      Visualization visualization =
          new Visualization.OneshotExpression(visualizationId, expressionId, contextId, expression);
      ctx.contextManager().upsertVisualization(contextId, visualization);

      var stack = ctx.contextManager().getStack(contextId);
      return new Executable(contextId, stack);
    } finally {
      ctx.locking().releaseContextLock(contextId);
      logger.log(
          Level.FINEST,
          "Kept context lock [{0}] for {1} milliseconds.",
          new Object[] {
            this.getClass().getSimpleName(), System.currentTimeMillis() - lockTimestamp
          });
    }
  }
}
