package org.enso.compiler.test.ir;

import static org.enso.compiler.test.ir.IRUtils.callArg;
import static org.enso.compiler.test.ir.IRUtils.defArg;
import static org.enso.compiler.test.ir.IRUtils.emptyIr;
import static org.enso.compiler.test.ir.IRUtils.literal;
import static org.enso.scala.wrapper.ScalaConversions.asScala;
import static org.enso.scala.wrapper.ScalaConversions.nil;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;

import java.util.ArrayList;
import java.util.List;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.test.pass.MockExpression;
import org.enso.compiler.test.pass.MockIR;
import org.enso.persist.Persistance.Reference;
import org.junit.Test;
import scala.Option;

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
  public void prefix_NoCopy() {
    var body = emptyIr();
    var xArg = callArg(literal("x"));
    var prefix =
        Application.Prefix.builder().arguments(asScala(List.of(xArg))).function(body).build();
    var mapped = prefix.mapExpressions(e -> e);
    assertThat("no copy should occur", mapped == prefix, is(true));
    assertThat("no copy of body", mapped.function() == body, is(true));
    var firstMappedArg = mapped.arguments().head();
    assertThat("no copy of argument", firstMappedArg == xArg, is(true));
  }

  @Test
  public void functionLambda_NoCopy() {
    var body = emptyIr();
    var self = literal("self");
    var selfArg = defArg(self);
    var lambda =
        Function.Lambda.builder()
            .bodyReference(Reference.of(body))
            .arguments(asScala(List.of(selfArg)))
            .build();
    var mappedLambda = lambda.mapExpressions(e -> e);
    assertThat("No copy should occur", mappedLambda == lambda, is(true));
    assertThat("No copy of body", mappedLambda.body() == body, is(true));
    var firstMappedArg = mappedLambda.arguments().head();
    assertThat("No copy of argument", firstMappedArg == selfArg, is(true));
  }

  /**
   * Name of {@link DefinitionArgument.Specified} is not collected, despite the fact that it is an
   * Expression.
   */
  @Test
  public void functionLambda_ArgumentName_IsNotCollected() {
    var body = emptyIr();
    var self = literal("self");
    var selfArg = defArg(self);
    var lambda =
        Function.Lambda.builder()
            .bodyReference(Reference.of(body))
            .arguments(asScala(List.of(selfArg)))
            .build();
    var collected = mapExpressions(lambda);
    assertThat("Only body is collected", collected.size(), is(1));
    assertThat("Body of Lambda is collected", collected, contains(body));
  }

  @Test
  public void functionLambda_ArgumentDefaultValue_IsCollected() {
    var body = emptyIr();
    var xLit = literal("x");
    var defaultValue = literal("some_default_value");
    var xArg =
        DefinitionArgument.Specified.builder()
            .name(xLit)
            .defaultValue(Option.apply(defaultValue))
            .ascribedType(Option.empty())
            .build();
    var lambda =
        Function.Lambda.builder()
            .bodyReference(Reference.of(body))
            .arguments(asScala(List.of(xArg)))
            .build();
    var collected = mapExpressions(lambda);
    assertThat(collected.size(), is(2));
    assertThat(collected, hasItem(defaultValue));
    assertThat(collected, hasItem(body));
    assertThat("Name of argument is not collected", collected, not(hasItem(xLit)));
  }

  @Test
  public void functionLambda_ArgumentAscribedType_IsCollected() {
    var body = emptyIr();
    var xLit = literal("x");
    var ascribedType = literal("Some_Ascribed_Type");
    var xArg =
        DefinitionArgument.Specified.builder()
            .name(xLit)
            .defaultValue(Option.empty())
            .ascribedType(Option.apply(ascribedType))
            .build();
    var lambda =
        Function.Lambda.builder()
            .bodyReference(Reference.of(body))
            .arguments(asScala(List.of(xArg)))
            .build();
    var collected = mapExpressions(lambda);
    assertThat(collected.size(), is(2));
    assertThat(collected, hasItem(ascribedType));
    assertThat(collected, hasItem(body));
    assertThat("Name of argument is not collected", collected, not(hasItem(xLit)));
  }

  @Test
  public void patternName_IsNotCollected() {
    var name = literal("name");
    var pat = Pattern.Name.create(name);
    var collected = mapExpressions(pat);
    assertThat("No expressions are collected", collected.isEmpty(), is(true));
  }

  private static List<Expression> mapExpressions(IR rootExpr) {
    var collected = new ArrayList<Expression>();
    rootExpr.mapExpressions(
        e -> {
          collected.add(e);
          return e;
        });
    return collected;
  }
}
