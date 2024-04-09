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
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class BinaryOpIntegerTest extends TestBase {
  private static final String[] OPERATIONS = {
    " +",
    " -",
    " ^",
    " *",
    " %",
    " ==",
    " <=",
    " <",
    " >=",
    " >",
    " /",
    ".div",
    ".bit_xor",
    ".bit_shift",
    ".bit_shift_r",
    ".bit_or",
    ".bit_and"
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
                      r.nextLong(),
                      switch (op) {
                        case " ^" -> r.nextLong(10);
                        case " *" -> r.nextLong(Integer.MAX_VALUE);
                        default -> r.nextLong();
                      }
                    });
    var zeroOps =
        Arrays.asList(OPERATIONS).stream().map((op) -> new Object[] {op, r.nextLong(), 0});
    var oneOps = Arrays.asList(OPERATIONS).stream().map((op) -> new Object[] {op, r.nextLong(), 1});
    var extraOps = Stream.of(new Object[] {" %", 19, 73}, new Object[] {".bit_shift", 12, 10});
    return Streams.concat(randomOps, zeroOps, oneOps, extraOps).toArray(Object[][]::new);
  }

  private static Context ctx;
  private static Value wrapInt;

  @BeforeClass
  public static void initContext() {
    ctx = createDefaultContext();
    wrapInt =
        ctx.eval(
                "enso",
                """
                from Standard.Base import all

                type Wrap
                  Int v

                  + self that:Wrap = self.v+that.v
                  * self that:Wrap = self.v*that.v
                  / self that:Wrap = self.v/that.v
                  - self that:Wrap = self.v-that.v
                  ^ self that:Wrap = self.v^that.v
                  % self that:Wrap = self.v%that.v
                  div self that:Wrap = self.v.div that.v
                  bit_xor self that:Wrap = self.v.bit_xor that.v
                  bit_shift self that:Wrap = self.v.bit_shift that.v
                  bit_shift_r self that:Wrap = self.v.bit_shift_r that.v
                  bit_or self that:Wrap = self.v.bit_or that.v
                  bit_and self that:Wrap = self.v.bit_and that.v
                  < self that:Wrap = self.v<that.v
                  <= self that:Wrap = self.v<=that.v
                  > self that:Wrap = self.v>that.v
                  >= self that:Wrap = self.v>=that.v

                Wrap.from(that:Integer) = Wrap.Int that

                wrap n:Integer -> Wrap = n
                """)
            .invokeMember(MethodNames.Module.EVAL_EXPRESSION, "wrap");
  }

  @AfterClass
  public static void closeContext() {
    ctx.close();
  }

  private final String operation;
  private final long n1;
  private final long n2;

  public BinaryOpIntegerTest(String operation, long n1, long n2) {
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

          var r1 = fn.execute(n1, n2);

          var wrap2 = ctx.asValue(new WrappedPrimitive(n2));
          var r2 = fn.execute(n1, wrap2);

          assertSameResult(r1, r2);
          return null;
        });
  }

  @Test
  public void verifyOperationWithConvertibleObject() {
    executeInContext(
        ctx,
        () -> {
          var code = """
        fn a b = a{op} b
        """.replace("{op}", operation);
          var fn = ctx.eval("enso", code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "fn");

          var r1 = fn.execute(n1, n2);

          if (operation.contains("=") || operation.contains("<") || operation.contains(">")) {
            // avoid any >=< for now
            return null;
          }

          if (!operation.startsWith(".")) {
            var wrap2 = wrapInt.execute(n2);
            var r2 = fn.execute(n1, wrap2);

            assertSameResult(r1, r2);
          }
          return null;
        });
  }

  @Test
  public void verifyOperationOnConvertibleObject() {
    executeInContext(
        ctx,
        () -> {
          var code = """
        fn a b = a{op} b
        """.replace("{op}", operation);
          var fn = ctx.eval("enso", code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "fn");

          var r1 = fn.execute(n1, n2);

          var wrap1 = wrapInt.execute(n1);
          var r2 = fn.execute(wrap1, n2);

          assertSameResult(r1, r2);
          return null;
        });
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
