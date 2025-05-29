package org.enso.compiler.test.pass;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.ArrayList;
import java.util.List;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.pass.MiniIRPass;

final class MockMiniPass extends MiniIRPass {
  private final MockExpression stopExpr;
  private final List<Expression> transformedExpressions = new ArrayList<>();

  /**
   * @param stopExpr When encountered this expression, {@code prepare} method will return null to
   *     signal that the traversal should stop. If null, this mini pass will not stop.
   */
  private MockMiniPass(MockExpression stopExpr) {
    this.stopExpr = stopExpr;
  }

  static Builder builder() {
    return new Builder();
  }

  @Override
  public Expression transformExpression(Expression expr) {
    if (expr instanceof MockExpression mockExpr) {
      assertThat(
          "Transform is called just once by one pass", mockExpr.isTransformedBy(this), is(false));
      mockExpr.setTransformedByPass(this);
    }
    transformedExpressions.add(expr);
    return expr;
  }

  @Override
  public MiniIRPass prepare(IR parent, Expression child) {
    if (child instanceof MockIR mockChild) {
      assertThat("prepare runs from top to bottom", mockChild.isPreparedBy(this), is(false));
    }
    if (parent instanceof MockIR mockParent) {
      mockParent.setPreparedBy(this);
    }
    if (stopExpr == child) {
      return null;
    }
    return this;
  }

  /**
   * Returns list of all the expressions that were transformed in {@link
   * #transformExpression(Expression)} method.
   */
  public List<Expression> getTransformedExpressions() {
    return transformedExpressions;
  }

  static final class Builder {
    private MockExpression stopExpr;

    Builder stopExpr(MockExpression stopExpr) {
      this.stopExpr = stopExpr;
      return this;
    }

    MockMiniPass build() {
      return new MockMiniPass(stopExpr);
    }
  }
}
