package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.dsl.UnsupportedSpecializationException;
import java.math.BigInteger;
import java.util.List;
import org.enso.interpreter.node.expression.builtin.number.integer.AbsNode;
import org.enso.interpreter.node.expression.builtin.number.integer.AddNode;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.graalvm.polyglot.Context;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

/**
 * Tests Truffle nodes for integer operations.
 */
@RunWith(Theories.class)
public class IntegerTest extends TestBase {

  private static AbsNode absNode;
  private static AddNode addNode;
  private static Context ctx;

  @BeforeClass
  public static void setup() {
    ctx = createDefaultContext();
    executeInContext(ctx, () -> {
      absNode = AbsNode.build();
      addNode = AddNode.build();
      return null;
    });
  }

  @DataPoints
  public static List<Object> datapoints = List.of(
      Long.MIN_VALUE,
      Long.MAX_VALUE,
      23,
      -23,
      23L,
      -23L,
      3.42,
      -3.42,
      new EnsoBigInteger(new BigInteger("1000000000000000000000000000000000000")),
      new EnsoBigInteger(new BigInteger("-10000000000000000000000000000000000"))
  );

  private static final EnsoBigInteger bigInt = new EnsoBigInteger(
      new BigInteger("1000000000000000000000000000000000000"));
  private static final EnsoBigInteger bigIntNegative = new EnsoBigInteger(
      new BigInteger("-1000000000000000000000000000000000000"));


  @Test
  public void testAbs() {
    executeInContext(ctx, () -> {
      assertEquals(23L, absNode.execute(23L));
      assertEquals(23L, absNode.execute(-23L));
      assertTrue(absNode.execute(Long.MIN_VALUE) instanceof EnsoBigInteger);
      assertEquals(bigInt, absNode.execute(bigInt));
      assertEquals(bigInt, absNode.execute(bigIntNegative));
      assertThrows("Decimals are not supported", UnsupportedSpecializationException.class, () -> absNode.execute(23.0));
      assertThrows("Java int is not supported", UnsupportedSpecializationException.class, () -> absNode.execute(23));
      return null;
    });
  }

  @Test
  public void testAdd() {

  }
}
