package org.enso.compiler.test.pass;

import static org.enso.compiler.test.ir.IRUtils.binaryOperator;
import static org.enso.compiler.test.ir.IRUtils.callArg;
import static org.enso.compiler.test.ir.IRUtils.defArg;
import static org.enso.compiler.test.ir.IRUtils.emptyIr;
import static org.enso.compiler.test.ir.IRUtils.literal;
import static org.enso.scala.wrapper.ScalaConversions.asScala;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.is;

import java.util.List;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.persist.Persistance.Reference;
import org.junit.Test;

public class MiniPassTraverserTest {
  @Test
  public void traversesOneExpression() {
    var expr = new MockExpression(null);
    var miniPass = MockMiniPass.builder().build();
    MiniIRPass.compile(MockExpression.class, expr, miniPass);
    assertThat(
        "Prepare is called only for trees with depth > 1", expr.isPreparedByAny(), is(false));
    assertThat(expr.isTransformedByAny(), is(true));
  }

  @Test
  public void traversesExpressionWithOneChild() {
    var parentExpr = new MockExpression(null);
    var childExpr = new MockExpression(parentExpr);
    var miniPass = MockMiniPass.builder().build();
    MiniIRPass.compile(MockExpression.class, parentExpr, miniPass);
    assertThat(parentExpr.isPreparedByAny(), is(true));
    assertThat(childExpr.isTransformedByAny(), is(true));
    assertThat(parentExpr.isTransformedByAny(), is(true));
  }

  @Test
  public void traversesExpressionWithManyChildren() {
    var parentExpr = new MockExpression(null);
    var children = List.of(new MockExpression(parentExpr), new MockExpression(parentExpr));
    var miniPass = MockMiniPass.builder().build();
    MiniIRPass.compile(MockExpression.class, parentExpr, miniPass);
    for (var ch : children) {
      assertThat(ch.isTransformedByAny(), is(true));
    }
    assertThat(parentExpr.isTransformedByAny(), is(true));
  }

  @Test
  public void traverseModule_SingleExpression() {
    var root = new MockExpression(null);
    var module = MockModule.createWithSingleMethod(root);
    var miniPass = MockMiniPass.builder().build();
    MiniIRPass.compile(Module.class, module, miniPass);
    assertThat(root.isTransformedBy(miniPass), is(true));
  }

  @Test
  public void traverseExpression_UnderNonExpression() {
    var root = new MockExpression(null);
    var child = new MockIR(root);
    var expr = new MockExpression(child);
    var miniPass = MockMiniPass.builder().build();
    MiniIRPass.compile(MockExpression.class, root, miniPass);
    assertThat(
        "Root is prepared - this is a known violation of IR.prepare contract. "
            + "Note that `child` should be prepared instead.",
        root.isPreparedBy(miniPass),
        is(true));
    assertThat(expr.isTransformedBy(miniPass), is(true));
    assertThat(root.isTransformedBy(miniPass), is(true));
  }

  @Test
  public void stopTraversingWhenPrepareReturnsNull() {
    var e1 = new MockExpression(null);
    var e2 = new MockExpression(e1);
    var e3 = new MockExpression(e2);
    // Should stop traversing when e3 is encountered.
    // Should only process e1 and e2, not e3
    var miniPass = MockMiniPass.builder().stopExpr(e3).build();
    MiniIRPass.compile(MockExpression.class, e1, miniPass);
    assertThat(e1.isPreparedByAny(), is(true));
    assertThat(e2.isPreparedByAny(), is(true));
    assertThat("e3 should not be processed - it is stopped expr", e3.isPreparedByAny(), is(false));
    assertThat(
        "e3 should not be processed - it is stopped expr", e3.isTransformedByAny(), is(false));
    assertThat("e2 should still be processed", e2.isTransformedByAny(), is(true));
  }

  @Test
  public void chainedMiniPass_TraversesSingleExpression() {
    var parentExpr = new MockExpression(null);
    var childExpr = new MockExpression(parentExpr);
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
        "Parent expression is prepared by both passes",
        parentExpr.isPreparedBy(miniPass1),
        is(true));
    assertThat(
        "Parent expression is prepared by both passes",
        parentExpr.isPreparedBy(miniPass2),
        is(true));
  }

  @Test
  public void chainedMiniPass_StopsTraversingWhenPrepareReturnsNull() {
    var e1 = new MockExpression(null);
    var e2 = new MockExpression(e1);
    var e3 = new MockExpression(e2);
    // miniPass1 stops traversing on e2.
    var miniPass1 = MockMiniPass.builder().stopExpr(e3).build();
    // miniPass2 traverses everything.
    var miniPass2 = MockMiniPass.builder().build();
    var chainedPass = MiniIRPass.combine(miniPass1, miniPass2);
    MiniIRPass.compile(MockExpression.class, e1, chainedPass);
    assertThat(
        "e3 should be transformed only by miniPass2", e3.isTransformedBy(miniPass2), is(true));
    assertThat("e3 must not be transformed by miniPass1", e3.isTransformedBy(miniPass1), is(false));
    assertThat(
        "e2 should still be transformed by miniPass1", e2.isTransformedBy(miniPass1), is(true));
  }

  @Test
  public void chainedMiniPass_StopsTraversingWhenPrepareFromBothPassesReturnNull() {
    var e1 = new MockExpression(null);
    var e2 = new MockExpression(e1);
    var e3 = new MockExpression(e2);
    // Both mini passes process just e1.
    var miniPass1 = MockMiniPass.builder().stopExpr(e2).build();
    var miniPass2 = MockMiniPass.builder().stopExpr(e2).build();
    var chainedPass = MiniIRPass.combine(miniPass1, miniPass2);
    MiniIRPass.compile(MockExpression.class, e1, chainedPass);
    assertThat("e3 should not be transformed by any pass", e3.isTransformedByAny(), is(false));
    assertThat("e2 should not be transformed by any pass", e2.isTransformedByAny(), is(false));
    assertThat("e1 should be processed by both passes", e1.isTransformedBy(miniPass1), is(true));
    assertThat("e1 should be processed by both passes", e1.isTransformedBy(miniPass2), is(true));
    assertThat("e1 should be prepared by both passes", e1.isPreparedBy(miniPass1), is(true));
    assertThat("e1 should be prepared by both passes", e1.isPreparedBy(miniPass2), is(true));
  }

  /** MiniPassTraverser ignores Case.Branch.pattern */
  @Test
  public void traverseOver_CaseExpression_IgnoresPattern() {
    var litX = literal("x");
    var litT = literal("T");
    var pattern = new Pattern.Type(litX, litT, null, new MetadataStorage());
    var empty1 = emptyIr();
    var empty2 = emptyIr();
    var branch = Case.Branch.builder().pattern(pattern).expression(empty1).build();
    var caseExpr = Case.Expr.builder().branches(asScala(List.of(branch))).scrutinee(empty2).build();
    var miniPass = MockMiniPass.builder().build();
    MiniIRPass.compile(Expression.class, caseExpr, miniPass);
    var visited = miniPass.getTransformedExpressions();
    assertThat(visited, containsInAnyOrder(empty1, empty2, caseExpr, branch));
  }

  @Test
  public void traverseOver_BinaryOperator() {
    var a = literal("a");
    var b = literal("b");
    var left = callArg(a);
    var right = callArg(b);
    var operator = literal("+");
    var binaryOperator = binaryOperator(left, right, operator);
    var miniPass = MockMiniPass.builder().build();
    MiniIRPass.compile(Expression.class, binaryOperator, miniPass);
    var visited = miniPass.getTransformedExpressions();
    assertThat(visited, containsInAnyOrder(a, b, binaryOperator, operator));
  }

  @Test
  public void traverseOver_FunctionLambda() {
    var body = emptyIr();
    var self = literal("self");
    var selfArg = defArg(self);
    var lambda =
        Function.Lambda.builder()
            .bodyReference(Reference.of(body))
            .arguments(scalaList(selfArg))
            .build();
    var miniPass = MockMiniPass.builder().build();
    MiniIRPass.compile(Expression.class, lambda, miniPass);
    var visited = miniPass.getTransformedExpressions();
    assertThat(visited, containsInAnyOrder(lambda, body));
  }

  private static <T> scala.collection.immutable.List<T> scalaList(T elem) {
    return asScala(List.of(elem));
  }
}
