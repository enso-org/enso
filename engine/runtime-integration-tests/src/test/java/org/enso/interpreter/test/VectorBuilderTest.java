package org.enso.interpreter.test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.warning.Warning;
import org.enso.interpreter.runtime.warning.WarningsLibrary;
import org.enso.interpreter.runtime.warning.WithWarnings;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class VectorBuilderTest {
  @ClassRule public static final ContextUtils ctx = ContextUtils.createDefault();
  private static Value create;

  @BeforeClass
  public static void initCreate() throws Exception {
    final URI srcUri = new URI("memory://longs.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
                import Standard.Base.Data.Vector.Builder
                from Standard.Base.Data.Vector import Vector

                create pw x y z =
                    b = Builder.new propagate_warnings=pw
                    b.append x
                    b.append y
                    b.append z
                    b.to_vector
                """,
                "choose.enso")
            .uri(srcUri)
            .buildLiteral();

    var module = ctx.eval(src);
    create = module.invokeMember("eval_expression", "create");
  }

  @AfterClass
  public static void releaseCreate() {
    create = null;
  }

  @Test
  public void buildVectorOfThreeLongs() throws Exception {
    var three = create.execute(false, 42, 17, -34);
    assertTrue("It is an array like structure", three.hasArrayElements());
    assertEquals(3, three.getArraySize());
    var raw = ctx.unwrapValue(three);
    var pkg = ArrayLikeHelpers.class.getPackage().getName();
    assertEquals("It is a long vector", pkg + ".Vector$Long", raw.getClass().getName());
  }

  @Test
  public void buildVectorOfThreeDoubles() throws Exception {
    var three = create.execute(false, Math.E, Math.PI, -Math.E * Math.PI);
    assertTrue("It is an array like structure", three.hasArrayElements());
    assertEquals(3, three.getArraySize());
    var raw = ctx.unwrapValue(three);
    var pkg = ArrayLikeHelpers.class.getPackage().getName();
    assertEquals("It is a double value", pkg + ".Vector$Double", raw.getClass().getName());
  }

  @Test
  public void buildVectorOfLongAndDoubles() throws Exception {
    var three = create.execute(false, 42, Math.PI, Math.E);
    assertTrue("It is an array like structure", three.hasArrayElements());
    assertEquals(3, three.getArraySize());
    var raw = ctx.unwrapValue(three);
    var pkg = ArrayLikeHelpers.class.getPackage().getName();
    // No special treatment for long & double:
    // assertEquals("It is a double value", pkg + ".Vector$Double", raw.getClass().getName());
    assertEquals(
        "No special long & double support. We switch to object array now.",
        pkg + ".Vector$EnsoOnly",
        raw.getClass().getName());
  }

  @Test
  public void buildVectorOfDoublesAndLongs() throws Exception {
    var three = create.execute(false, Math.E, 42, Math.PI);
    assertTrue("It is an array like structure", three.hasArrayElements());
    assertEquals(3, three.getArraySize());
    var raw = ctx.unwrapValue(three);
    var pkg = ArrayLikeHelpers.class.getPackage().getName();
    // No special treatment for long & double:
    // assertEquals("It is a double value", pkg + ".Vector$Double", raw.getClass().getName());
    assertEquals(
        "No special long & double support. We switch to object array now.",
        pkg + ".Vector$EnsoOnly",
        raw.getClass().getName());
  }

  @Test
  public void buildVectorOfLongAndOneWarning() throws Exception {
    var warn = Warning.create(ctx.ensoContext(), Text.create("Problematic"), 0);
    var ww = WithWarnings.create(17L, 1, false, warn);
    var three = create.execute(true, 42, ww, -34);
    assertTrue("It is an array like structure", three.hasArrayElements());
    assertEquals(3, three.getArraySize());
    var raw = ctx.unwrapValue(three);
    var pkg = ArrayLikeHelpers.class.getPackage().getName();
    if (raw instanceof WithWarnings with) {
      raw = with.getValue();
      assertEquals("It is complex array", pkg + ".Vector$Long", raw.getClass().getName());
      assertTrue("Has warnings", WarningsLibrary.getUncached().hasWarnings(with));
      var warningsRaw = WarningsLibrary.getUncached().getWarnings(with, false);
      assertEquals("EnsoHashMap", "" + warningsRaw.getClass().getSimpleName());
      var warnings = ctx.context().asValue(warningsRaw);
      assertTrue("It is a hash", warnings.hasHashEntries());
      assertEquals("There is one hash", 1, warnings.getHashSize());
      var firstWarning = warnings.getHashValuesIterator().getIteratorNextElement();
      var firstWarningRaw = ctx.unwrapValue(firstWarning);
      assertEquals("It is our warning", warn, firstWarningRaw);
    } else {
      fail("There should be a warning associted with " + raw);
    }
    var arr = three.as(long[].class);
    assertArrayEquals("Still it can be converted to long array", new long[] {42, 17, -34}, arr);
  }

  @Test
  public void buildVectorOfLongAndOneWarningDontPropagate() throws Exception {
    var warn = Warning.create(ctx.ensoContext(), Text.create("Problematic"), 0);
    var ww = WithWarnings.create(17L, 1, false, warn);
    var three = create.execute(false, 42, ww, -34);
    assertTrue("It is an array like structure", three.hasArrayElements());
    assertEquals(3, three.getArraySize());
    var raw = ctx.unwrapValue(three);
    assertFalse(
        "There are no warnings on the raw value", WarningsLibrary.getUncached().hasWarnings(raw));
    var pkg = ArrayLikeHelpers.class.getPackage().getName();
    assertEquals("It is complex array", pkg + ".Vector$Long", raw.getClass().getName());
    var arr = three.as(long[].class);
    assertArrayEquals("Still it can be converted to long array", new long[] {42, 17, -34}, arr);
  }

  @Test
  public void buildVectorOfThreeTexts() throws Exception {
    var fourtyTwo = Text.create("42");
    var seventeen = Text.create("17");
    var thirtyFourMinus = Text.create("-34");
    var three = create.execute(false, fourtyTwo, seventeen, thirtyFourMinus);
    assertTrue("It is an array like structure", three.hasArrayElements());
    assertEquals(3, three.getArraySize());
    var raw = ctx.unwrapValue(three);
    var pkg = ArrayLikeHelpers.class.getPackage().getName();
    assertEquals("It is a long value", pkg + ".Vector$EnsoOnly", raw.getClass().getName());
  }

  @Test
  public void buildVectorOfTextsAndOneWarning() throws Exception {
    var fourtyTwo = Text.create("42");
    var seventeen = Text.create("17");
    var thirtyFourMinus = Text.create("-34");
    var warn = Warning.create(ctx.ensoContext(), Text.create("Problematic"), 0);
    var ww = WithWarnings.create(seventeen, 1, false, warn);
    var three = create.execute(true, fourtyTwo, ww, thirtyFourMinus);
    assertTrue("It is an array like structure", three.hasArrayElements());
    assertEquals(3, three.getArraySize());
    var raw = ctx.unwrapValue(three);
    var pkg = ArrayLikeHelpers.class.getPackage().getName();
    if (raw instanceof WithWarnings with) {
      raw = with.getValue();
      assertEquals("It is complex array", pkg + ".Vector$EnsoOnly", raw.getClass().getName());
      assertTrue("Has warnings", WarningsLibrary.getUncached().hasWarnings(with));
      var warningsRaw = WarningsLibrary.getUncached().getWarnings(with, false);
      assertEquals("EnsoHashMap", "" + warningsRaw.getClass().getSimpleName());
      var warnings = ctx.context().asValue(warningsRaw);
      assertTrue("It is a hash", warnings.hasHashEntries());
      assertEquals("There is one hash", 1, warnings.getHashSize());
      var firstWarning = warnings.getHashValuesIterator().getIteratorNextElement();
      var firstWarningRaw = ctx.unwrapValue(firstWarning);
      assertEquals("It is our warning", warn, firstWarningRaw);
    } else {
      fail("There should be a warning associted with " + raw);
    }
    var arr = three.as(Object[].class);
    assertArrayEquals(
        "Still it can be converted to String array", new Object[] {"42", "17", "-34"}, arr);
  }

  @Test
  public void buildVectorOfTextsAndOneWarningDontPropagate() throws Exception {
    var fourtyTwo = Text.create("42");
    var seventeen = Text.create("17");
    var thirtyFourMinus = Text.create("-34");
    var warn = Warning.create(ctx.ensoContext(), Text.create("Problematic"), 0);
    var ww = WithWarnings.create(seventeen, 1, false, warn);
    var three = create.execute(false, fourtyTwo, ww, thirtyFourMinus);
    assertTrue("It is an array like structure", three.hasArrayElements());
    assertEquals(3, three.getArraySize());
    var raw = ctx.unwrapValue(three);
    var pkg = ArrayLikeHelpers.class.getPackage().getName();
    assertFalse(
        "There are no warnings on the raw value", WarningsLibrary.getUncached().hasWarnings(raw));
    assertEquals("It is complex array", pkg + ".Vector$EnsoOnly", raw.getClass().getName());
    var arr = three.as(Object[].class);
    assertArrayEquals(
        "Still it can be converted to String array", new Object[] {"42", "17", "-34"}, arr);
  }
}
