package org.enso.compiler.test.ir;

import static org.enso.compiler.test.ir.IRUtils.defArg;
import static org.enso.compiler.test.ir.IRUtils.emptyIr;
import static org.enso.compiler.test.ir.IRUtils.literal;
import static org.enso.scala.wrapper.ScalaConversions.asScala;
import static org.enso.scala.wrapper.ScalaConversions.nil;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.hasItem;

import java.util.ArrayList;
import java.util.List;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.test.pass.MockExpression;
import org.enso.compiler.test.pass.MockIR;
import org.enso.persist.Persistance.Reference;
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

  @Test
  public void prefix() {
    var empty = emptyIr();
    var prefix = Application.Prefix.builder().function(empty).arguments(nil()).build();
    var collected = mapExpressions(prefix);
    assertThat(collected, hasItem(empty));
  }

  @Test
  public void caseExpr() {
    var empty = emptyIr();
    var caseExpr = Case.Expr.builder().scrutinee(empty).branches(nil()).build();
    var collected = mapExpressions(caseExpr);
    assertThat(collected, hasItem(empty));
  }

  @Test
  public void functionLambda() {
    var body = emptyIr();
    var self = literal("self");
    var selfArg = defArg(self);
    var lambda =
        Function.Lambda.builder()
            .bodyReference(Reference.of(body))
            .arguments(asScala(List.of(selfArg)))
            .build();
    var collected = mapExpressions(lambda);
    assertThat(collected, hasItem(body));
    assertThat(collected, hasItem(self));
  }

  private static List<Expression> mapExpressions(Expression rootExpr) {
    var collected = new ArrayList<Expression>();
    rootExpr.mapExpressions(
        e -> {
          collected.add(e);
          return e;
        });
    return collected;
  }
}
