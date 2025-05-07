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
   *     signal that the traversal should stop. Can be null.
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
      if (mockExpr.hasParent()) {
        assertThat(
            "Prepare must be called on an expression with a parent",
            mockExpr.isPreparedBy(this),
            is(true));
      }
      assertThat(
          "Transform is called just once by one pass", mockExpr.isTransformedBy(this), is(false));
      mockExpr.setTransformedByPass(this);
    }
    transformedExpressions.add(expr);
    return expr;
  }

  @Override
  public MiniIRPass prepare(IR parent, Expression child) {
    if (stopExpr == child) {
      return null;
    }
    if (child instanceof MockExpression mockExpr) {
      assertThat("Prepare is called just once by one pass", mockExpr.isPreparedBy(this), is(false));
      mockExpr.setPreparedBy(this);
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
