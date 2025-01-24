package org.enso.interpreter.test.interop;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.interop.InteropLibrary;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URI;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.function.Predicate;
import org.enso.common.MethodNames;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.type.ConstantsGen;
import org.enso.interpreter.test.ValuesGenerator;
import org.enso.interpreter.test.ValuesGenerator.Language;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class MetaObjectTest {
  private static Context ctx;
  private static Value sn;
  private static ValuesGenerator generator;

  @BeforeClass
  public static void prepareCtx() throws IOException {
    ctx = ContextUtils.createDefaultContext();
    var code =
        """
        from Standard.Base import Meta, Error

        sn v = v.to Meta.Type . catch handler=(_-> Meta.meta Error) . name
        """;
    var src = Source.newBuilder("enso", code, "simple_name.enso").build();
    sn = ctx.eval(src).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "sn");
    assertTrue("It is a function", sn.canExecute());
  }

  @AfterClass
  public static void disposeCtx() {
    if (generator != null) {
      generator.dispose();
      generator = null;
    }
    ctx.close();
    ctx = null;
    sn = null;
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
  public void checkingAtomMetaObject() throws Exception {
    final URI uri = new URI("memory://callback.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    type Atm
        Data x
        End

    data = Atm.Data 5
    end = Atm.End
    """,
                "atom_test.enso")
            .uri(uri)
            .buildLiteral();

    var module = ctx.eval(src);

    var data = module.invokeMember("eval_expression", "data");
    assertFalse("Non-null result", data.isNull());
    var meta = data.getMetaObject();
    assertNotNull("Non-null meta for " + data, meta);
    assertFalse("Non-null type", meta.isNull());
    assertTrue("Is meta", meta.isMetaObject());
    assertEquals("Atm", meta.getMetaSimpleName());
    assertEquals("atom_test.Atm", meta.getMetaQualifiedName());
    assertFalse("Isn't instance of itself", meta.isMetaInstance(meta));
    assertTrue("Is instance", meta.isMetaInstance(data));
    assertTrue("Eigen-type", meta.getMetaObject().isMetaObject());
    assertEquals("Eigen-type is equal", meta.getMetaObject(), meta.getMetaObject().getMetaObject());

    var end = module.invokeMember("eval_expression", "end");
    assertEquals("Same meta", end.getMetaObject(), data.getMetaObject());
  }

  @Test
  public void checkAllConstantGenValuesArePresent() throws Exception {
    var g = generator();
    var expecting = new HashSet<String>();
    for (var f : ConstantsGen.class.getFields()) {
      if (!f.getName().endsWith("_BUILTIN")) {
        var s = (String) f.get(null);
        expecting.add(s);
      }
    }
    expecting.remove(ConstantsGen.ARRAY_LIKE_HELPERS);
    var w = new StringBuilder();
    var f = new StringWriter();
    var err = new PrintWriter(f);
    for (var t : g.allTypes()) {
      if (t.isNull()) {
        expecting.remove("Standard.Base.Nothing.Nothing");
        continue;
      }
      try {
        var n = t.getMetaQualifiedName();
        assertNotNull("Type " + t + " has meta name", n);
        if (!expecting.remove(n)) {
          w.append("\nNo type removed for ").append(n);
        }
      } catch (AssertionError ex) {
        err.println(ex.getMessage());
      }
    }

    if (f.toString().length() > 0) {
      fail(f.toString());
    }

    if (!expecting.isEmpty()) {
      fail("Should be empty: " + expecting + w);
    }
  }

  @Test
  public void checkAllTypesHaveSomeValues() throws Exception {
    checkAllTypesSatisfy(MetaObjectTest::checkValue);
  }

  @Test
  public void checkAllTypesHaveInstanceOfValues() throws Exception {
    checkAllTypesSatisfy(MetaObjectTest::checkIsInstance);
  }

  @Test
  public void warningIsTransparent() {
    ValuesGenerator g = ValuesGenerator.create(ctx, ValuesGenerator.Language.ENSO);
    for (var v : g.warnings()) {
      assertTrue("Warning is string: " + v, v.isString());
      assertEquals("value", v.asString());
      assertEquals("Warning " + v + " has type Text", g.typeText(), v.getMetaObject());
    }
  }

  @Test
  public void checkArraysAreArrays() {
    var g = ValuesGenerator.create(ctx, ValuesGenerator.Language.ENSO);
    for (var v : g.arrayLike()) {
      var isVector = v.getMetaObject().equals(g.typeVector());
      var isArray = v.getMetaObject().equals(g.typeArray());
      assertTrue(
          "Value "
              + v
              + " of type "
              + v.getMetaObject()
              + " should either be array or vector ("
              + isVector
              + ")",
          isArray ^ isVector);
    }
  }

  @Test
  public void errorsAreWeird() {
    var g = ValuesGenerator.create(ctx, ValuesGenerator.Language.ENSO);
    for (var v : g.errors()) {
      Value vMeta = v.getMetaObject();
      var isError = vMeta.equals(g.typeError());
      var isPanic = vMeta.equals(g.typePanic());

      assertTrue("either error or panic: " + v, isError || isPanic);
      assertNotEquals("never both: " + v, isError, isPanic);

      if (isError) {
        assertFalse("No meta parents for errors: " + vMeta, vMeta.hasMetaParents());
      } else {
        assertTrue("There are meta parents for panics: " + vMeta, vMeta.hasMetaParents());
        var arr = vMeta.getMetaParents();
        for (long i = 0; i < arr.getArraySize(); i++) {
          var p = arr.getArrayElement(i);
          assertEquals(g.typeAny(), p);
        }
      }
    }
  }

  @Test
  public void typesOfConstructors() throws Exception {
    var g = generator();
    var types = new java.util.HashSet<Value>();
    for (var c : g.constructorsAndValuesAndSumType()) {
      if (c.isMetaObject()) {
        types.add(c);
      }
    }
    for (var c : g.constructorsAndValuesAndSumType()) {
      if (c.isMetaObject() || types.contains(c.getMetaObject())) {
        continue;
      }
      assertNotNull("c " + c + " has a type", c.getMetaObject());
      assertEquals("c " + c + " is function", "Function", c.getMetaObject().getMetaSimpleName());
      assertEquals("c " + c + " is function", g.typeFunction(), c.getMetaObject());
    }
  }

  @Test
  public void nothingIsNotMeta() {
    var g = ValuesGenerator.create(ctx, ValuesGenerator.Language.ENSO);
    var nothing = g.typeNothing();
    assertThat("Nothing is not meta", nothing.isMetaObject(), is(false));
  }

  @Test
  public void nothingWithWarningIsNotMeta() {
    var src =
        """
import Standard.Base.Warning.Warning
import Standard.Base.Nothing.Nothing

main = Warning.attach "foo" Nothing
""";
    var nothingWithWarn = ContextUtils.evalModule(ctx, src);
    assertThat(nothingWithWarn.isMetaObject(), is(false));
  }

  @Test
  public void nothingShouldBeNull() {
    var src = """
import Standard.Base.Nothing.Nothing
main = Nothing
""";
    var nothing = ContextUtils.evalModule(ctx, src);
    assertThat(nothing.isNull(), is(true));
  }

  @Test
  public void numbersAreEitherIntegerOrFloat() throws Exception {
    var g = generator();
    for (var v : g.numbers()) {
      var simpleName = sn.execute(v).asString();
      var ok =
          switch (simpleName) {
            case "Integer", "Float" -> true;
            default -> false;
          };
      assertTrue("Unexpected simple name for number: " + v + " is " + simpleName, ok);
    }
  }

  /**
   * Primitive values and exceptions currently don't have an associated language.
   *
   * <p>TODO[PM]: Will be implemented in https://github.com/enso-org/enso/pull/11468
   */
  @Test
  public void allEnsoNonPrimitiveValuesHaveLanguage() throws Exception {
    var gen = ValuesGenerator.create(ctx, Language.ENSO);
    Predicate<Value> isPrimitiveOrException =
        (val) -> val.fitsInInt() || val.fitsInDouble() || val.isBoolean() || val.isException();
    var nonPrimitiveValues =
        gen.allValues().stream().filter(isPrimitiveOrException.negate()).toList();
    var interop = InteropLibrary.getUncached();
    ContextUtils.executeInContext(
        ctx,
        () -> {
          for (var value : nonPrimitiveValues) {
            var unwrappedValue = ContextUtils.unwrapValue(ctx, value);
            assertThat(
                "Value " + unwrappedValue + " should have associated language",
                interop.hasLanguage(unwrappedValue),
                is(true));
          }
          return null;
        });
  }

  @Test
  public void compareQualifiedAndSimpleTypeName() throws Exception {
    var g = generator();
    var sb = new StringBuilder();
    for (var v : g.allValues()) {
      compareQualifiedNameOfValue(sn, v, sb);
    }
    if (!sb.isEmpty()) {
      var lines = sb.toString().lines().count() - 1;
      sb.insert(0, "There is " + lines + " differences:");
      fail(sb.toString());
    }
  }

  private boolean compareQualifiedNameOfValue(Value sn, Value v, StringBuilder sb) {
    var simpleName = sn.execute(v).asString();
    if (v.isNumber()) {
      var ok =
          switch (simpleName) {
            case "Integer", "Float" -> true;
            default -> false;
          };
      assertTrue("Unexpected simple name for number: " + simpleName, ok);
      return true;
    }
    var meta = v.getMetaObject();
    var metaName = meta != null ? meta.getMetaSimpleName() : "null";
    if (!simpleName.equals(metaName)) {
      if (v.isHostObject()) {
        if (v.hasArrayElements()) {
          assertEquals("Array", simpleName);
          return true;
        }
        if (v.hasHashEntries()) {
          assertEquals("Dictionary", simpleName);
          return true;
        }
      }
      if (v.hasMembers() && v.getMember("__proto__") != null) {
        if (v.hasHashEntries()) {
          assertEquals("JavaScript hash map is called Map", "Map", metaName);
          assertEquals(
              "JavaScript hash map is seen as Dictionary by Enso", "Dictionary", simpleName);
          return true;
        }
      }
      if (v.isString()) {
        assertEquals("Text", simpleName);
        return true;
      }
      if (v.isDuration()) {
        assertEquals("Duration", simpleName);
        return true;
      }
      if (v.isDate() && v.isTime()) {
        assertEquals("Date_Time", simpleName);
        return true;
      }
      if (v.isTimeZone()) {
        assertEquals("Time_Zone", simpleName);
        return true;
      }
      if (v.isDate()) {
        assertEquals("Date", simpleName);
        return true;
      }
      if (v.isTime()) {
        assertEquals("Time_Of_Day", simpleName);
        return true;
      }
      if (v.isNull()) {
        assertEquals("Nothing", simpleName);
        return true;
      }
      sb.append("\n")
          .append("Simple names shall be the same for ")
          .append(v)
          .append(" Enso simple name: ")
          .append(simpleName)
          .append(" getMetaSimpleName: ")
          .append(metaName);
    }
    return false;
  }

  @Test
  public void compareQualifiedAndSimpleTypeNameForTypes() throws Exception {
    var g = generator();
    var sb = new StringBuilder();
    for (var typ : g.allTypes()) {
      if (!typ.isMetaObject()) {
        // skip Nothing
        continue;
      }
      var type = (Type) ContextUtils.unwrapValue(ctx, typ);
      if (type.isEigenType()) {
        // Skip singleton types
        continue;
      }

      var simpleName = sn.execute(typ).asString();
      var metaName = typ.getMetaSimpleName() + ".type";
      if (!simpleName.equals(metaName)) {
        sb.append("\n")
            .append("Simple names shall be the same for ")
            .append(typ)
            .append(" Enso simple name: ")
            .append(simpleName)
            .append(" getMetaSimpleName: ")
            .append(metaName);
      }
    }
    if (!sb.isEmpty()) {
      var lines = sb.toString().lines().count() - 1;
      sb.insert(0, "There is " + lines + " differences:");
      fail(sb.toString());
    }
  }

  private void checkAllTypesSatisfy(Check check) throws Exception {
    var g = generator();
    var expecting = new LinkedHashSet<Value>();
    for (var t : g.allTypes()) {
      if (t.isNull()) {
        continue;
      }
      var type = (Type) ContextUtils.unwrapValue(ctx, t);
      if (type.isEigenType()) {
        // Skip checking singleton types
        continue;
      }
      switch (t.getMetaSimpleName()) {
          // represented as primitive values without meta object
        case "Float" -> {}
          // has no instances
        case "Array_Proxy" -> {}
          // Warning is transparent and invisible
        case "Warning" -> {}
        default -> expecting.add(t);
      }
    }
    var successfullyRemoved = new HashSet<Value>();
    var w = new StringBuilder();
    for (var v : g.allValues()) {
      check.check(v, null, expecting, successfullyRemoved, w);
    }
    if (!expecting.isEmpty()) {
      fail("These types don't have any values: " + expecting + w);
    }
  }

  @FunctionalInterface
  interface Check {

    /**
     * @param v Instance of the type
     * @param type Type. Nullable.
     * @param expecting Set of types that are tested. The check should remove the currently tested
     *     type from this set.
     * @param successfullyRemoved Set of types that already were tested. The check should add the
     *     currently tested type to this set.
     * @param w StringBuilder for the error message that will be printed at the end in case of a
     *     failure.
     */
    void check(
        Value v, Value type, Set<Value> expecting, Set<Value> successfullyRemoved, StringBuilder w);
  }

  private static void checkValue(
      Value v, Value type, Set<Value> expecting, Set<Value> successfullyRemoved, StringBuilder w) {
    var t = type == null ? v.getMetaObject() : type;
    if (t == null) {
      return;
    }
    if (!expecting.remove(t)) {
      if (!successfullyRemoved.contains(t)) {
        w.append("\nCannot remove type ").append(t).append(" for value ").append(v);
      }
    } else {
      successfullyRemoved.add(t);
    }
    if (t.hasMetaParents() && t.getMetaParents() instanceof Value p && p.hasArrayElements()) {
      for (long i = 0; i < p.getArraySize(); i++) {
        checkValue(v, p.getArrayElement(i), expecting, successfullyRemoved, w);
      }
    }
  }

  private static void checkIsInstance(
      Value v, Value nullT, Set<Value> expecting, Set<Value> successfullyRemoved, StringBuilder w) {
    for (var type : new LinkedHashSet<>(expecting)) {
      if (!type.isMetaInstance(v)) {
        continue;
      }
      if (!expecting.remove(type)) {
        if (!successfullyRemoved.contains(type)) {
          w.append("\nCannot remove type ").append(type).append(" for value ").append(v);
        }
      } else {
        successfullyRemoved.add(type);
      }
    }
  }
}
