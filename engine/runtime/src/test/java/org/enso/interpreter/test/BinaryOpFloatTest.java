package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.google.common.collect.Streams;
import java.util.Arrays;
import java.util.Random;
import java.util.stream.Stream;
import org.enso.polyglot.MethodNames;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class BinaryOpFloatTest extends TestBase {
  private static final String[] OPERATIONS = {
    " +", " -", " ^", " *", " %", " <=", " <", " >=", " >", " /"
  };

  @Parameterized.Parameters(name = "({1}){0} ({2})")
  public static Object[][] parameters() {
    var r = new Random();
    var randomOps =
        Arrays.asList(OPERATIONS).stream()
            .map(
                (op) ->
                    new Object[] {
                      op,
                      r.nextDouble(),
                      switch (op) {
                        case " ^" -> r.nextDouble(10);
                        case " *" -> r.nextDouble(Integer.MAX_VALUE);
                        default -> r.nextDouble();
                      }
                    });
    var zeroOps =
        Arrays.asList(OPERATIONS).stream().map((op) -> new Object[] {op, r.nextDouble(), 0.0});
    var oneOps =
        Arrays.asList(OPERATIONS).stream().map((op) -> new Object[] {op, r.nextDouble(), 1.0});
    var extraOps = Stream.of(new Object[] {" %", 19.73, 12.10}, new Object[] {" ^", 10.12, 73.19});
    return Streams.concat(randomOps, zeroOps, oneOps, extraOps).toArray(Object[][]::new);
  }

  private static Context ctx;

  @BeforeClass
  public static void initContext() {
    ctx = createDefaultContext();
  }

  @AfterClass
  public static void closeContext() {
    ctx.close();
  }

  private final String operation;
  private final double n1;
  private final double n2;

  public BinaryOpFloatTest(String operation, double n1, double n2) {
    this.operation = operation;
    this.n1 = n1;
    this.n2 = n2;
  }

  @Test
  public void verifyOperationOnForeignObject() {
    executeInContext(
        ctx,
        () -> {
          var code = """
        fn a b = a{op} b
        """.replace("{op}", operation);
          var fn = ctx.eval("enso", code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "fn");

          var r1 = execute(fn, n1, n2);

          var wrap2 = ctx.asValue(new WrappedPrimitive(n2));
          var r2 = execute(fn, n1, wrap2);

          assertSameResult(r1, r2);
          return null;
        });
  }

  private Value execute(Value fn, Object... args) {
    try {
      return fn.execute(args);
    } catch (PolyglotException ex) {
      return ex.getGuestObject();
    }
  }

  private void assertSameResult(Value r1, Value r2) {
    assertEquals("r1: " + r1 + " r2: " + r2, r1.isException(), r2.isException());
    assertEquals("r1: " + r1 + " r2: " + r2, r1.isBoolean(), r2.isBoolean());
    assertEquals("r1: " + r1 + " r2: " + r2, r1.fitsInLong(), r2.fitsInLong());
    assertEquals("r1: " + r1 + " r2: " + r2, r1.fitsInDouble(), r2.fitsInDouble());
    assertEquals("r1: " + r1 + " r2: " + r2, r1.fitsInBigInteger(), r2.fitsInBigInteger());

    if (r1.fitsInLong()) {
      assertEquals("Results for " + n1 + operation + " " + n2, r1.asLong(), r2.asLong());
    } else if (r1.fitsInDouble()) {
      assertEquals("Results for " + n1 + operation + " " + n2, r1.asDouble(), r2.asDouble(), 0.1);
    } else if (r1.fitsInBigInteger()) {
      assertEquals(
          "Results for " + n1 + operation + " " + n2, r1.asBigInteger(), r2.asBigInteger());
    } else if (r1.isBoolean()) {
      assertEquals("Results for " + n1 + operation + " " + n2, r1.asBoolean(), r2.asBoolean());
    } else if (r1.isException()) {
      assertTrue("Both are exceptions for " + n1 + operation + " " + n2, r2.isException());
    } else {
      fail("Doesn't fit: " + r1);
    }
  }
}
