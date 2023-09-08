package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import org.enso.interpreter.node.expression.builtin.number.integer.AbsNode;
import org.enso.interpreter.node.expression.builtin.number.integer.AddNode;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.graalvm.polyglot.Context;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.experimental.theories.Theories;
import org.junit.runner.RunWith;

/** Tests Truffle nodes for integer operations. */
@RunWith(Theories.class)
public class IntegerTest extends TestBase {

  private static AbsNode absNode;
  private static AddNode addNode;
  private static Context ctx;

  @BeforeClass
  public static void setup() {
    ctx = createDefaultContext();
    executeInContext(
        ctx,
        () -> {
          absNode = AbsNode.build();
          addNode = AddNode.build();
          return null;
        });
  }

  private static final EnsoBigInteger bigInt =
      new EnsoBigInteger(new BigInteger("1000000000000000000000000000000000000"));
  private static final EnsoBigInteger bigIntNegative =
      new EnsoBigInteger(new BigInteger("-1000000000000000000000000000000000000"));

  @Test
  public void testAbs() {
    executeInContext(
        ctx,
        () -> {
          assertEquals(23L, absNode.execute(23L));
          assertEquals(23L, absNode.execute(-23L));
          assertTrue(absNode.execute(Long.MIN_VALUE) instanceof EnsoBigInteger);
          assertEquals(bigInt, absNode.execute(bigInt));
          assertEquals(bigInt, absNode.execute(bigIntNegative));
          assertThrows(
              "Decimals are not supported", PanicException.class, () -> absNode.execute(23.0));
          assertThrows(
              "Java int is not supported", PanicException.class, () -> absNode.execute(23));
          return null;
        });
  }

  @Test
  public void testAdd() {
    executeInContext(
        ctx,
        () -> {
          assertEquals(23L, addNode.execute(22L, 1L));
          assertThrows(PanicException.class, () -> addNode.execute(23L, "Hello"));
          return null;
        });
  }
}
