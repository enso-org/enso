package org.enso.interpreter.node.expression.builtin.number.decimal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

import java.math.BigInteger;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.test.WrappedPrimitive;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/** Tests Truffle nodes for integer operations. */
public class FloatTest {

  private static AbsNode absNode;
  private static AddNode addNode;
  private static TestRootNode root;
  private static Context ctx;

  @BeforeClass
  public static void setup() {
    ctx = ContextUtils.createDefaultContext();
    ContextUtils.executeInContext(
        ctx,
        () -> {
          absNode = AbsNode.build();
          addNode = AddNode.build();

          root = new TestRootNode();
          root.insertChildren(absNode, addNode);
          return null;
        });
  }

  @AfterClass
  public static void teardown() {
    ctx.close();
    ctx = null;
  }

  private static final EnsoBigInteger bigInt =
      new EnsoBigInteger(new BigInteger("1000000000000000000000000000000000000"));
  private static final EnsoBigInteger bigIntNegative =
      new EnsoBigInteger(new BigInteger("-1000000000000000000000000000000000000"));

  @Test
  public void testAbs23() {
    ContextUtils.executeInContext(
        ctx,
        () -> {
          assertEquals(23.1, absNode.execute(23.1), 0.01);
          assertEquals(23.1, absNode.execute(-23.1), 0.01);
          return null;
        });
  }

  @Test
  public void testAdd21And1Point0() {
    ContextUtils.executeInContext(
        ctx,
        () -> {
          assertEquals(23.1, addNode.execute(22.0, 1.1), 0.01);
          return null;
        });
  }

  @Test
  public void testAdd21And1() {
    ContextUtils.executeInContext(
        ctx,
        () -> {
          assertEquals(23.1, addNode.execute(22.1, 1L), 0.01);
          return null;
        });
  }

  @Test
  public void testAddMulti21And1() {
    ContextUtils.executeInContext(
        ctx,
        () -> {
          var nn = EnsoMultiValue.NewNode.getUncached();
          var leak = ContextUtils.leakContext(ctx);
          var floatType = leak.getBuiltins().number().getFloat();
          var textType = leak.getBuiltins().text();
          var both = new Type[] {floatType, textType};
          var twentyTwoHello = nn.newValue(both, 2, 0, new Object[] {22.1, "Hello"});
          assertEquals(23.2, addNode.execute(1.1, twentyTwoHello), 0.01);
          return null;
        });
  }

  @Test
  public void testAddInterop21And1() {
    ContextUtils.executeInContext(
        ctx,
        () -> {
          var twentyOne = new WrappedPrimitive(21.1);
          assertEquals(23.1, addNode.execute(2.0, twentyOne), 0.01);
          return null;
        });
  }

  @Test
  public void testAddDoubleAndText() {
    ContextUtils.executeInContext(
        ctx,
        () -> {
          assertThrows(PanicException.class, () -> addNode.execute(23.1, "Hello"));
          return null;
        });
  }
}
