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
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

/** Tests Truffle nodes for integer operations. */
public class FloatTest {

  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();
  private static AbsNode absNode;
  private static AddNode addNode;
  private static TestRootNode root;

  @BeforeClass
  public static void setup() {
    absNode = AbsNode.build();
    addNode = AddNode.build();

    root = new TestRootNode();
    root.insertChildren(absNode, addNode);
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
    assertEquals(23.1, absNode.execute(23.1), 0.01);
    assertEquals(23.1, absNode.execute(-23.1), 0.01);
  }

  @Test
  public void testAdd21And1Point0() {
    assertEquals(23.1, addNode.execute(22.0, 1.1), 0.01);
  }

  @Test
  public void testAdd21And1() {
    assertEquals(23.1, addNode.execute(22.1, 1L), 0.01);
  }

  @Test
  public void testAddMulti21And1() {
    var nn = EnsoMultiValue.NewNode.getUncached();
    var leak = ctxRule.ensoContext();
    var floatType = leak.getBuiltins().number().getFloat();
    var textType = leak.getBuiltins().text();
    var both = new Type[] {floatType, textType};
    var twentyTwoHello = nn.newValue(both, 2, 0, new Object[] {22.1, "Hello"});
    assertEquals(23.2, addNode.execute(1.1, twentyTwoHello), 0.01);
  }

  @Test
  public void testAddInterop21And1() {
    var twentyOne = new WrappedPrimitive(21.1);
    assertEquals(23.1, addNode.execute(2.0, twentyOne), 0.01);
  }

  @Test
  public void testAddDoubleAndText() {
    assertThrows(PanicException.class, () -> addNode.execute(23.1, "Hello"));
  }
}
