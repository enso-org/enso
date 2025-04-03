package org.enso.interpreter.node.expression.builtin.number.integer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.test.WrappedPrimitive;
import org.enso.test.utils.ContextRule;
import org.enso.test.utils.TestRootNode;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

/** Tests Truffle nodes for integer operations. */
public class IntegerTest {

  @ClassRule public static final ContextRule ctxRule = ContextRule.createDefault();
  private static AbsNode absNode;
  private static AddNode addNode;
  private static TestRootNode root;

  @BeforeClass
  public static void setup() {
    ctxRule.executeInContext(
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
    absNode = null;
    addNode = null;
    root = null;
  }

  private static final EnsoBigInteger bigInt =
      new EnsoBigInteger(new BigInteger("1000000000000000000000000000000000000"));
  private static final EnsoBigInteger bigIntNegative =
      new EnsoBigInteger(new BigInteger("-1000000000000000000000000000000000000"));

  @Test
  public void testAbs23() {
    ctxRule.executeInContext(
        () -> {
          assertEquals(23L, absNode.execute(23L));
          assertEquals(23L, absNode.execute(-23L));
          return null;
        });
  }

  @Test
  public void testAbsBig() {
    ctxRule.executeInContext(
        () -> {
          assertTrue(absNode.execute(Long.MIN_VALUE) instanceof EnsoBigInteger);
          assertEquals(bigInt, absNode.execute(bigInt));
          assertEquals(bigInt, absNode.execute(bigIntNegative));
          return null;
        });
  }

  @Test
  public void testAbsPanic() {
    ctxRule.executeInContext(
        () -> {
          assertThrows(
              "Decimals are not supported", PanicException.class, () -> absNode.execute(23.0));
          assertThrows(
              "Java int is not supported", PanicException.class, () -> absNode.execute(23));
          return null;
        });
  }

  @Test
  public void testAdd21And1() {
    ctxRule.executeInContext(
        () -> {
          assertEquals(23L, addNode.execute(22L, 1L));
          return null;
        });
  }

  @Test
  public void testAdd21And1Point0() {
    ctxRule.executeInContext(
        () -> {
          assertEquals(23.1, ((Number) addNode.execute(22L, 1.1)).doubleValue(), 0.01);
          return null;
        });
  }

  @Test
  public void testAddMulti21And1() {
    ctxRule.executeInContext(
        () -> {
          var nn = EnsoMultiValue.NewNode.getUncached();
          var leak = ctxRule.leakContext();
          var intType = leak.getBuiltins().number().getInteger();
          var textType = leak.getBuiltins().text();
          var both = new Type[] {intType, textType};
          var twentyTwoHello = nn.newValue(both, 2, 0, new Object[] {22L, "Hello"});
          assertEquals(23L, addNode.execute(twentyTwoHello, 1L));
          return null;
        });
  }

  @Test
  public void testAddInterop21And1() {
    ctxRule.executeInContext(
        () -> {
          var twentyOne = new WrappedPrimitive(21L);
          assertEquals(23L, addNode.execute(twentyOne, 2L));
          return null;
        });
  }

  @Test
  public void testAddLongAndText() {
    ctxRule.executeInContext(
        () -> {
          assertThrows(PanicException.class, () -> addNode.execute(23L, "Hello"));
          return null;
        });
  }
}
