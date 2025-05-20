package org.enso.compiler.test.pass;

import static org.enso.scala.wrapper.ScalaConversions.asScala;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;

import java.util.List;
import org.enso.compiler.core.ir.CallArgument;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Empty;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.expression.Operator;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.persist.Persistance.Reference;
import org.junit.Test;
import scala.Option;

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
    expectVisited(miniPass, empty2);
    expectVisited(miniPass, empty1);
    expectVisited(miniPass, caseExpr);
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
    expectVisited(miniPass, a);
    expectVisited(miniPass, b);
    expectVisited(miniPass, binaryOperator);
  }

  @Test
  public void traverseOver_FunctionLambda() {
    var body = binaryOperator();
    var lambda =
        Function.Lambda.builder()
            .bodyReference(Reference.of(body))
            .arguments(scalaList(selfArg()))
            .build();
    var miniPass = MockMiniPass.builder().build();
    MiniIRPass.compile(Expression.class, lambda, miniPass);
    expectVisited(miniPass, lambda);
    expectVisited(miniPass, body);
  }

  private static void expectVisited(MockMiniPass pass, Expression expectedVisitedExpr) {
    var transformedExpressions = pass.getTransformedExpressions();
    var transformedExprClasses =
        transformedExpressions.stream().map(e -> e.getClass().getName()).toList();
    assertThat(
        "Expected expression of type '"
            + expectedVisitedExpr.getClass().getName()
            + "' to be visited, but visited expressions were: "
            + transformedExprClasses,
        transformedExpressions,
        hasItem(expectedVisitedExpr));
  }

  private static Name.Literal literal(String lit) {
    return new Name.Literal(lit, false, null, Option.empty(), new MetadataStorage());
  }

  private static Empty emptyIr() {
    return Empty.builder().build();
  }

  private static DefinitionArgument.Specified selfArg() {
    return DefinitionArgument.Specified.builder()
        .name(literal("self"))
        .ascribedType(Option.empty())
        .defaultValue(Option.empty())
        .build();
  }

  private static Operator.Binary binaryOperator() {
    return new Operator.Binary(
        callArg("a"), literal("+"), callArg("b"), null, new MetadataStorage());
  }

  private static Operator.Binary binaryOperator(
      CallArgument left, CallArgument right, Name operator) {
    return new Operator.Binary(left, operator, right, null, new MetadataStorage());
  }

  private static CallArgument.Specified callArg(String name) {
    return CallArgument.Specified.builder().value(literal(name)).name(Option.empty()).build();
  }

  private static CallArgument.Specified callArg(Name name) {
    return CallArgument.Specified.builder().value(name).name(Option.empty()).build();
  }

  private static <T> scala.collection.immutable.List<T> scalaList(T elem) {
    return asScala(List.of(elem));
  }
}
