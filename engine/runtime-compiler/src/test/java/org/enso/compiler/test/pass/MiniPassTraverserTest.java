package org.enso.compiler.test.pass;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.List;
import org.enso.compiler.pass.MiniIRPass;
import org.junit.Test;

public class MiniPassTraverserTest {
  @Test
  public void traversesOneExpression() {
    var expr = new MockExpression(false);
    var miniPass = MockMiniPass.builder().build();
    MiniIRPass.compile(MockExpression.class, expr, miniPass);
    assertThat(
        "Prepare is called only for trees with depth > 1", expr.isPreparedByAny(), is(false));
    assertThat(expr.isTransformedByAny(), is(true));
  }

  @Test
  public void traversesExpressionWithOneChild() {
    var parentExpr = new MockExpression(false);
    var childExpr = new MockExpression(true);
    parentExpr.addChild(childExpr);
    var miniPass = MockMiniPass.builder().build();
    MiniIRPass.compile(MockExpression.class, parentExpr, miniPass);
    assertThat(
        "Prepare must be called on a child expression", childExpr.isPreparedByAny(), is(true));
    assertThat(childExpr.isTransformedByAny(), is(true));
    assertThat(parentExpr.isTransformedByAny(), is(true));
  }

  @Test
  public void traversesExpressionWithManyChildren() {
    var parentExpr = new MockExpression(false);
    var children = List.of(new MockExpression(true), new MockExpression(true));
    children.forEach(parentExpr::addChild);
    var miniPass = MockMiniPass.builder().build();
    MiniIRPass.compile(MockExpression.class, parentExpr, miniPass);
    for (var ch : children) {
      assertThat("Prepare must be called on a child expression", ch.isPreparedByAny(), is(true));
      assertThat(ch.isTransformedByAny(), is(true));
    }
    assertThat(parentExpr.isTransformedByAny(), is(true));
  }

  @Test
  public void stopTraversingWhenPrepareReturnsNull() {
    var e1 = new MockExpression(false);
    var e2 = new MockExpression(true);
    var e3 = new MockExpression(true);
    e1.addChild(e2);
    e2.addChild(e3);
    // Should stop traversing when e3 is encountered.
    // Should only process e1 and e2, not e3
    var miniPass = MockMiniPass.builder().stopExpr(e3).build();
    MiniIRPass.compile(MockExpression.class, e1, miniPass);
    assertThat("e3 should not be processed", e3.isPreparedByAny(), is(false));
    assertThat("e3 should not be processed", e3.isTransformedByAny(), is(false));
    assertThat("e2 should still be processed", e2.isPreparedByAny(), is(true));
    assertThat("e2 should still be processed", e2.isTransformedByAny(), is(true));
  }

  @Test
  public void chainedMiniPass_TraversesSingleExpression() {
    var parentExpr = new MockExpression(false);
    var childExpr = new MockExpression(true);
    parentExpr.addChild(childExpr);
    var miniPass1 = MockMiniPass.builder().build();
    var miniPass2 = MockMiniPass.builder().build();
    var chainedPass = MiniIRPass.combine(miniPass1, miniPass2);
    MiniIRPass.compile(MockExpression.class, parentExpr, chainedPass);
    assertThat(
        "Child expression is transformed by both passes",
        childExpr.isTransformedBy(miniPass1),
        is(true));
    assertThat(
        "Child expression is transformed by both passes",
        childExpr.isTransformedBy(miniPass2),
        is(true));
    assertThat(
        "Child expression is prepared by both passes", childExpr.isPreparedBy(miniPass1), is(true));
    assertThat(
        "Child expression is prepared by both passes", childExpr.isPreparedBy(miniPass2), is(true));
  }

  @Test
  public void chainedMiniPass_StopsTraversingWhenPrepareReturnsNull() {
    var e1 = new MockExpression(false);
    var e2 = new MockExpression(true);
    var e3 = new MockExpression(true);
    e1.addChild(e2);
    e2.addChild(e3);
    // miniPass1 stops traversing on e2.
    var miniPass1 = MockMiniPass.builder().stopExpr(e3).build();
    // miniPass2 traverses everything.
    var miniPass2 = MockMiniPass.builder().build();
    var chainedPass = MiniIRPass.combine(miniPass1, miniPass2);
    MiniIRPass.compile(MockExpression.class, e1, chainedPass);
    assertThat("e3 should be prepared only by miniPass2", e3.isPreparedBy(miniPass2), is(true));
    assertThat(
        "e3 should be transformed only by miniPass2", e3.isTransformedBy(miniPass2), is(true));
    assertThat("e3 must not be transformed by miniPass1", e3.isTransformedBy(miniPass1), is(false));
    assertThat(
        "e2 should still be transformed by miniPass1", e2.isTransformedBy(miniPass1), is(true));
  }

  @Test
  public void chainedMiniPass_StopsTraversingWhenPrepareFromBothPassesReturnNull() {
    var e1 = new MockExpression(false);
    var e2 = new MockExpression(true);
    var e3 = new MockExpression(true);
    e1.addChild(e2);
    e2.addChild(e3);
    // Both mini passes process just e1.
    var miniPass1 = MockMiniPass.builder().stopExpr(e2).build();
    var miniPass2 = MockMiniPass.builder().stopExpr(e2).build();
    var chainedPass = MiniIRPass.combine(miniPass1, miniPass2);
    MiniIRPass.compile(MockExpression.class, e1, chainedPass);
    assertThat("e3 should not be prepared by any pass", e3.isPreparedByAny(), is(false));
    assertThat("e3 should not be transformed by any pass", e3.isTransformedByAny(), is(false));
    assertThat("e2 should not be prepared by any pass", e2.isPreparedByAny(), is(false));
    assertThat("e2 should not be transformed by any pass", e2.isTransformedByAny(), is(false));
    assertThat("e1 should be processed by both passes", e1.isTransformedBy(miniPass1), is(true));
    assertThat("e1 should be processed by both passes", e1.isTransformedBy(miniPass2), is(true));
  }
}
