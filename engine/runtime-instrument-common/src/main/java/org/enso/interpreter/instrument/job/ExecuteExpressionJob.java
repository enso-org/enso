package org.enso.interpreter.instrument.job;

import java.util.UUID;
import org.enso.interpreter.instrument.OneshotExpression;
import org.enso.interpreter.instrument.execution.Executable;
import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.scala.wrapper.ScalaConversions;

/** The job that schedules the execution of the expression. */
public class ExecuteExpressionJob extends Job<Executable> implements UniqueJob<Executable> {

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
    super(ScalaConversions.cons(contextId, ScalaConversions.nil()), true, false, true);
    this.contextId = contextId;
    this.visualizationId = visualizationId;
    this.expressionId = expressionId;
    this.expression = expression;
  }

  @Override
  public Executable runImpl(RuntimeContext ctx) {
    return ctx.locking()
        .withContextLock(
            ctx.locking().getOrCreateContextLock(contextId),
            this.getClass(),
            () -> {
              OneshotExpression oneshotExpression =
                  new OneshotExpression(visualizationId, expressionId, contextId, expression);
              ctx.contextManager().setOneshotExpression(contextId, oneshotExpression);

              var stack = ctx.contextManager().getStack(contextId);
              return new Executable(contextId, stack);
            });
  }

  @Override
  public boolean equalsTo(UniqueJob<?> that) {
    if (that instanceof ExecuteExpressionJob job) {
      return contextId == job.contextId && expressionId == job.expressionId;
    }
    return false;
  }
}
