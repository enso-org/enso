package org.enso.compiler.test.ir;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;

import java.util.ArrayList;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.test.pass.MockExpression;
import org.enso.compiler.test.pass.MockIR;
import org.junit.Test;

public class MapExpressionsTest {
  /**
   * Contract for mapExpressions: It traverses over the nearest Expression children and stops there.
   */
  @Test
  public void mapExpressions_Contract() {
    var root = new MockExpression(null);
    var child = new MockIR(root);
    var nestedChild = new MockIR(child);
    var expr = new MockExpression(nestedChild);
    var collected = new ArrayList<Expression>();
    root.mapExpressions(
        e -> {
          collected.add(e);
          return e;
        });
    assertThat(collected, contains(expr));
  }
}
