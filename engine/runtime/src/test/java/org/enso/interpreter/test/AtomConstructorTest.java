package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.function.Function;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.atom.AtomNewInstanceNode;
import org.enso.interpreter.runtime.data.atom.StructsLibrary;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.polyglot.MethodNames;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author devel
 */
public class AtomConstructorTest extends TestBase {

  private static Context ctx;

  public AtomConstructorTest() {}

  @BeforeClass
  public static void initContext() {
    ctx = createDefaultContext();
  }

  @AfterClass
  public static void closeContext() {
    ctx.close();
  }

  @Test
  public void newInstance() {
    var code = """
        type X
            A a b c
        """;
    var module = ctx.eval("enso", code);
    var xA = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "X.A");
    var raw = unwrapValue(ctx, xA);

    assertTrue("It is atom constructor: " + raw, raw instanceof AtomConstructor);
    var cons = (AtomConstructor) raw;

    assertAtomFactory("AtomConstructor.newInstance", cons::newInstance);
    assertLessArguments("AtomConstructor.newInstance", cons::newInstance);
  }

  @Test
  public void newAtomNewInstanceNode() {
    var code = """
        type X
            A a b c
        """;
    var module = ctx.eval("enso", code);
    var xA = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "X.A");
    var raw = unwrapValue(ctx, xA);

    assertTrue("It is atom constructor: " + raw, raw instanceof AtomConstructor);
    var cons = (AtomConstructor) raw;

    var node = AtomNewInstanceNode.create();
    Function<Object[], Atom> factory = args -> node.execute(cons, args);
    assertAtomFactory("AtomConstructor.newInstance", factory);
    assertLessArguments("AtomConstructor.newInstance", factory);
  }

  private static void assertAtomFactory(String msg, Function<Object[], Atom> factory) {
    var boxed = factory.apply(new Object[] {"a", "b", "c"});
    assertEquals(msg + " all texts", "BoxingAtom", boxed.getClass().getSimpleName());
    assertValues(msg, boxed, "a", "b", "c");

    var long0 = factory.apply(new Object[] {1L, "b", "c"});
    assertEquals(
        msg + " long first", "UnboxingAtom", long0.getClass().getSuperclass().getSimpleName());
    assertEquals(msg + " long first", "Layout_Atom_1_2", long0.getClass().getSimpleName());
    assertValues(msg, long0, 1L, "b", "c");

    var long1 = factory.apply(new Object[] {"a", 2L, "c"});
    assertEquals(
        msg + " long first", "UnboxingAtom", long1.getClass().getSuperclass().getSimpleName());
    assertEquals(msg + " long second", "Layout_Atom_1_2", long1.getClass().getSimpleName());
    assertValues(msg, long1, "a", 2L, "c");

    var long2 = factory.apply(new Object[] {"a", "b", 3L});
    assertEquals(
        msg + " long first", "UnboxingAtom", long2.getClass().getSuperclass().getSimpleName());
    assertEquals(msg + " long third", "Layout_Atom_1_2", long2.getClass().getSimpleName());
    assertValues(msg, long2, "a", "b", 3L);

    var longDoubleText = factory.apply(new Object[] {1L, 2.0, "c"});
    assertEquals(
        msg + " long first",
        "UnboxingAtom",
        longDoubleText.getClass().getSuperclass().getSimpleName());
    assertEquals(msg + " long first", "Layout_Atom_2_1", longDoubleText.getClass().getSimpleName());
    assertValues(msg, longDoubleText, 1L, 2.0, "c");

    var doubleTextLong = factory.apply(new Object[] {1.0, "b", 3L});
    assertEquals(
        msg + " long first",
        "UnboxingAtom",
        doubleTextLong.getClass().getSuperclass().getSimpleName());
    assertEquals(msg + " long first", "Layout_Atom_2_1", doubleTextLong.getClass().getSimpleName());
    assertValues(msg, doubleTextLong, 1.0, "b", 3L);

    var longLongLong = factory.apply(new Object[] {1L, 2L, 3L});
    assertEquals(
        msg + " long first",
        "UnboxingAtom",
        longLongLong.getClass().getSuperclass().getSimpleName());
    assertEquals(msg + " long first", "Layout_Atom_3_0", longLongLong.getClass().getSimpleName());
    assertValues(msg, longLongLong, 1L, 2L, 3L);
  }

  private static void assertLessArguments(String msg, Function<Object[], Atom> factory) {
    executeInContext(
        ctx,
        () -> {
          try {
            var zero = factory.apply(new Object[0]);
            fail("Expecting exception: " + zero);
          } catch (PanicException e) {
            assertEquals(msg + " no arguments", "Arity_Error.Error", e.getMessage());
          }
          try {
            var one = factory.apply(new Object[] {"a"});
            fail("Expecting exception: " + one);
          } catch (PanicException e) {
            assertEquals(msg + " one argument", "Arity_Error.Error", e.getMessage());
          }
          try {
            var two = factory.apply(new Object[] {"a", "b"});
            fail("Expecting exception: " + two);
          } catch (PanicException e) {
            assertEquals(msg + " two arguments", "Arity_Error.Error", e.getMessage());
          }
          return null;
        });
  }

  private static void assertValues(String msg, Atom atom, Object... values) {
    var l = StructsLibrary.getUncached();
    for (int i = 0; i < values.length; i++) {
      var v = l.getField(atom, i);
      assertEquals(msg + " at " + i, values[i], v);
    }
  }
}
