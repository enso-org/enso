package org.enso.interpreter.test;

import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class MetaIsATest extends TestBase {
  private Context ctx;
  private Value isACheck;

  @Before
  public void prepareCtx() throws Exception {
    ctx = createDefaultContext();
    final URI uri = new URI("memory://choose.enso");
    final Source src = Source.newBuilder("enso", """
    import Standard.Base.Meta

    check x y = Meta.is_a x y
    """, "check.enso")
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    isACheck = module.invokeMember("eval_expression", "check");
    assertTrue("it is a function", isACheck.canExecute());
  }

  @After
  public void disposeCtx() {
    ctx.close();
  }

  @Test
  public void checkNumbersAreNumber() {
    var g = ValuesGenerator.create(ctx);
    var typeNumber = g.typeNumber();
    for (var v : g.numbers()) {
      var r = isACheck.execute(v, typeNumber);
      assertTrue("Value " + v + " is a number, type: " + v.getMetaObject(), r.asBoolean());
    }
  }

  @Test
  public void checkValuesAreAny() throws Exception {
    var g = ValuesGenerator.create(ctx);
    var typeAny = g.typeAny();
    for (var v : g.allValues()) {
      var r = isACheck.execute(v, typeAny);
      assertTrue("Value " + v + " is any", r.asBoolean());
    }
  }

  @Test
  public void checkNumbersAreNotText() {
    var g = ValuesGenerator.create(ctx);
    for (var v : g.numbers()) {
      var r = isACheck.execute(v, g.typeText());
      assertFalse("Value " + v + " is not a string", r.asBoolean());
    }
  }

  @Test
  public void checkTextsAreText() {
    var g = ValuesGenerator.create(ctx);
    for (var v : g.textual()) {
      var r = isACheck.execute(v, g.typeText());
      assertTrue("Value " + v + " is a string", r.asBoolean());
    }
  }

  @Test
  public void checkIntegerIsNotInstanceOfInteger() {
    var g = ValuesGenerator.create(ctx);
    var t = g.typeInteger();
    var r = isACheck.execute(t, t);
    assertFalse("Integer is not instance of Integer", r.asBoolean());
  }

  @Test
  public void checkNumberIsNotInstanceOfNumber() {
    var g = ValuesGenerator.create(ctx);
    var t = g.typeNumber();
    var r = isACheck.execute(t, t);
    assertFalse("Number is not instance of Number", r.asBoolean());
  }

  @Test
  public void checkAnyIsInstanceOfAny() {
    var g = ValuesGenerator.create(ctx);
    var t = g.typeAny();
    var r = isACheck.execute(t, t);
    assertTrue("Everything is instance of Any even Any", r.asBoolean());
  }

  @Test
  public void checkTextsAreNotNumbers() {
    var g = ValuesGenerator.create(ctx);
    for (var v : g.textual()) {
      var r = isACheck.execute(v, g.typeNumber());
      assertFalse("Value " + v + " is not a number", r.asBoolean());
    }
  }

  @Test
  public void checkArraysAreArrays() {
    var g = ValuesGenerator.create(ctx);
    for (var v : g.arrayLike()) {
      var isVector = isACheck.execute(v, g.typeVector());
      var isArray = isACheck.execute(v, g.typeArray());
      assertTrue("Value " + v + " of type " + v.getMetaObject() + " should either be array (" + isArray + ") or vector (" + isVector + ")", isArray.asBoolean() ^ isVector.asBoolean());
    }
  }

  @Test
  public void valuesAreNotInstancesOfThemselves() throws Exception {
    var g = ValuesGenerator.create(ctx);
    for (var v : g.allValues()) {
      var r = isACheck.execute(v, v);
      if (v.equals(g.typeNothing())) {
        assertTrue("Nothing is instance of itself", r.asBoolean());
      } else {
        assertFalse("Value " + v + " shall not be instance of itself", r.isBoolean() && r.asBoolean());
      }
    }
  }

  @Test
  public void constructorVariants() throws Exception {
    var g = ValuesGenerator.create(ctx);
    var found = new HashMap<Value, Value>();
    final List<Value> values = g.constructorsAndValuesAndSumType();
    for (var v1 : values) {
      for (var v2 : values) {
        assertTypeWithCheck(g, v1, v2, found);
      }
    }
    assertEquals("Just one: " + found, 1, found.size());
  }

  private void assertTypeWithCheck(ValuesGenerator g, Value type, Value value, Map<Value, Value> found) {
    var r = isACheck.execute(value, type);
    Value withTypeCaseOf;
    try {
       withTypeCaseOf = g.withType(type);
    } catch (IllegalArgumentException ex) {
      assertFalse("It is not a type: " + type + " value: " + value, r.asBoolean());
      return;
    }
    var is = withTypeCaseOf.execute(value);
    assertTrue("Returns boolean for " + withTypeCaseOf, is.fitsInInt());
    if (r.asBoolean()) {
      assertEquals("True is 1 for " + type + " check of " + value, 1, is.asInt());
      found.put(type, value);
    } else {
        assertEquals("False is 0 for " + type + " check of " + value, 0, is.asInt());
    }
  }

  @Test
  public void typesAreNotInstancesOfThemselves() throws Exception {
    var g = ValuesGenerator.create(ctx);
    var f = new StringBuilder();
    for (var v : g.allTypes()) {
      if (v.equals(g.typeAny())) {
        continue;
      }
      var r = isACheck.execute(v, v);
      if (v.equals(g.typeNothing())) {
        assertTrue("Nothing is instance of itself", r.asBoolean());
      } else {
        if (r.asBoolean()) {
          f.append("\nType ").append(v).append(" shall not be instance of itself");
        }
      }
    }
    assertEquals(f.toString(), 0, f.length());
  }

  @Test
  public void consistencyWithCase() throws Exception {
    var g = ValuesGenerator.create(ctx);
    var f = new StringBuilder();
    for (var t : g.allTypes()) {
      var typeCaseOf = g.withType(t);

      for (var v : g.allValues()) {
        assertTypeAndValue(typeCaseOf, v, t, f, g);
      }
    }
    if (f.length() > 0) {
      fail(f.toString());
    }
  }

  private void assertTypeAndValue(Value caseOf, Value v, Value t, StringBuilder f, ValuesGenerator g) {
    var test = caseOf.execute(v);
    if (test.isException()) {
      assertEquals("DataFlowError in", g.typeError(), v.getMetaObject());
      assertEquals("DataFlowError out", g.typeError(), test.getMetaObject());
      return;
    }
    assertTrue("Expecting 0 or 1 result: " + test + " for " + v, test.isNumber());
    var testBool = test.asInt() == 1;
    var res = isACheck.execute(v, t);
    assertTrue(res.isBoolean());
    if (res.asBoolean() != testBool) {
      f.append("\nType ").append(t).append(" and value ").append(v).
        append(" caseof: ").append(test).append(" Meta.is_a ").append(res);
    }
  }
}
