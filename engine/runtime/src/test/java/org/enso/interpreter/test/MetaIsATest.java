package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.enso.interpreter.test.ValuesGenerator.Language;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class MetaIsATest extends TestBase {
  private static Context ctx;
  private static Value isACheck;
  private static ValuesGenerator generator;

  @BeforeClass
  public static void prepareCtx() throws Exception {
    ctx = createDefaultContext();
    final URI uri = new URI("memory://choose.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    import Standard.Base.Meta

    check x y = Meta.is_a x y
    """,
                "check.enso")
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);
    isACheck = module.invokeMember("eval_expression", "check");
    assertTrue("it is a function", isACheck.canExecute());
  }

  @AfterClass
  public static void disposeCtx() {
    if (generator != null) {
      generator.dispose();
    }
    ctx.close();
  }

  /**
   * Override to create different values generator.
   *
   * @param context the context to allocate values in
   * @return an instance of values generator
   */
  ValuesGenerator createGenerator(Context context) {
    return ValuesGenerator.create(context, Language.ENSO, Language.JAVA);
  }

  private ValuesGenerator generator() {
    if (generator == null) {
      generator = createGenerator(ctx);
    }
    return generator;
  }

  @Test
  public void checkNumbersAreNumber() {
    var typeNumber = generator().typeNumber();
    for (var v : generator().numbers()) {
      var r = isACheck.execute(v, typeNumber);
      assertTrue("Value " + v + " is a number, type: " + v.getMetaObject(), r.asBoolean());
    }
  }

  @Test
  public void checkValuesAreAny() throws Exception {
    var typeAny = generator().typeAny();
    for (var v : generator().allValues()) {
      var r = isACheck.execute(v, typeAny);
      assertTrue("Value " + v + " is any", r.asBoolean());
    }
  }

  @Test
  public void checkNumbersAreNotText() {
    for (var v : generator().numbers()) {
      var r = isACheck.execute(v, generator().typeText());
      assertFalse("Value " + v + " is not a string", r.asBoolean());
    }
  }

  @Test
  public void checkTextsAreText() {
    for (var v : generator().textual()) {
      var r = isACheck.execute(v, generator().typeText());
      assertTrue("Value " + v + " is a string", r.asBoolean());
    }
  }

  private void assertType(String msg, Value value, Value... types) {
    var error = new StringBuilder();
    for (var type : types) {
      var r = isACheck.execute(value, type);
      if (r.asBoolean()) {
        return;
      }
      var typeName = value.getMetaObject().getMetaQualifiedName();
      error
          .append("\n")
          .append(msg)
          .append(", but value ")
          .append(value)
          .append(" has type ")
          .append(typeName);
    }
    fail(error.toString());
  }

  @Test
  public void mapsAreMaps() {
    for (var v : generator().maps()) {
      assertType("Expecting a map", v, generator().typeMap());
    }
  }

  @Test
  public void datesAreDates() {
    for (var v : generator().timesAndDates()) {
      assertType(
          "Expecting a date",
          v,
          generator().typeDate(),
          generator().typeDateTime(),
          generator().typeTimeOfDay(),
          generator().typeDuration(),
          generator().typePeriod(),
          generator().typeTimePeriod(),
          generator().typeDatePeriod());
    }
  }

  @Test
  public void zonesAreZones() {
    for (var v : generator().timeZones()) {
      assertType("Expecting time zone", v, generator().typeTimeZone());
    }
  }

  @Test
  public void checkIntegerIsNotInstanceOfInteger() {
    var t = generator().typeInteger();
    var r = isACheck.execute(t, t);
    assertFalse("Integer is not instance of Integer", r.asBoolean());
  }

  @Test
  public void checkNumberIsNotInstanceOfNumber() {
    var t = generator().typeNumber();
    var r = isACheck.execute(t, t);
    assertFalse("Number is not instance of Number", r.asBoolean());
  }

  @Test
  public void checkAnyIsInstanceOfAny() {
    var t = generator().typeAny();
    var r = isACheck.execute(t, t);
    assertTrue("Everything is instance of Any even Any", r.asBoolean());
  }

  @Test
  public void checkTextsAreNotNumbers() {
    for (var v : generator().textual()) {
      var r = isACheck.execute(v, generator().typeNumber());
      assertFalse("Value " + v + " is not a number", r.asBoolean());
    }
  }

  @Test
  public void checkArraysAreArrays() {
    for (var v : generator().arrayLike()) {
      var isVector = isACheck.execute(v, generator().typeVector());
      var isArray = isACheck.execute(v, generator().typeArray());
      assertTrue(
          "Value "
              + v
              + " of type "
              + v.getMetaObject()
              + " should either be array ("
              + isArray
              + ") or vector ("
              + isVector
              + ")",
          isArray.asBoolean() ^ isVector.asBoolean());
    }
  }

  @Test
  public void valuesAreNotInstancesOfThemselves() throws Exception {
    for (var v : generator().allValues()) {
      var r = isACheck.execute(v, v);
      if (v.equals(generator().typeNothing())) {
        assertTrue("Nothing is instance of itself", r.asBoolean());
      } else {
        assertFalse(
            "Value " + v + " shall not be instance of itself", r.isBoolean() && r.asBoolean());
      }
    }
  }

  @Test
  public void constructorVariants() throws Exception {
    var found = new HashMap<Value, Value>();
    final List<Value> values = generator().constructorsAndValuesAndSumType();
    for (var v1 : values) {
      for (var v2 : values) {
        assertTypeWithCheck(generator, v1, v2, found);
      }
    }
    assertEquals("Just one: " + found, 1, found.size());
  }

  private void assertTypeWithCheck(
      ValuesGenerator g, Value type, Value value, Map<Value, Value> found) {
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
    var f = new StringBuilder();
    for (var v : generator().allTypes()) {
      if (v.equals(generator().typeAny())) {
        continue;
      }
      var r = isACheck.execute(v, v);
      if (v.equals(generator().typeNothing())) {
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
    var f = new StringBuilder();
    for (var t : generator().allTypes()) {
      var typeCaseOf = generator().withType(t);

      for (var v : generator().allValues()) {
        assertTypeAndValue(typeCaseOf, v, t, f, generator);
      }
    }
    if (f.length() > 0) {
      fail(f.toString());
    }
  }

  private void assertTypeAndValue(
      Value caseOf, Value v, Value t, StringBuilder f, ValuesGenerator g) {
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
      f.append("\nType ")
          .append(t)
          .append(" and value ")
          .append(v)
          .append(" caseof: ")
          .append(test)
          .append(" Meta.is_a ")
          .append(res);
    }
  }
}
