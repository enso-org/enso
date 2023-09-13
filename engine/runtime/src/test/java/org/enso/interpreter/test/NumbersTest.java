package org.enso.interpreter.test;

import java.util.Random;

import org.enso.polyglot.MethodNames;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.BeforeClass;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.FromDataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

@RunWith(Theories.class)
public class NumbersTest extends TestBase {

  @DataPoints("operation")
  public static final String[] OPERATION = {
    " +", " -", " ^", " *", " %", " <=", " <", " >=", " >", " /",
    ".div", ".bit_xor", ".bit_shift", ".bit_shift_r", ".bit_or", ".bit_and"
  };
  private static Context ctx;

  @BeforeClass
  public static void initContext() {
    ctx = createDefaultContext();
  }

  @AfterClass
  public static void closeContext() {
    ctx.close();
  }

  @Theory
  public void verifyOperationOnForeignObject(
          @FromDataPoints("operation") String operation
  ) {
    executeInContext(ctx, () -> {
      var code = """
        fn a b = a{op} b
        """.replace("{op}", operation);
      var fn = ctx.eval("enso", code).invokeMember(MethodNames.Module.EVAL_EXPRESSION, "fn");

      var r = new Random();
      var n1 = r.nextInt();
      var n2 = r.nextInt();
      System.out.println(n1 + operation + " " + n2);

      var r1 = fn.execute(n1, n2);

      var wrap2 = ctx.asValue(new WrappedPrimitive(n2));
      var r2 = fn.execute(n1, wrap2);

      assertEquals("r1: " + r1 + " r2: " + r2, r1.isBoolean(), r2.isBoolean());
      assertEquals("r1: " + r1 + " r2: " + r2, r1.fitsInLong(), r2.fitsInLong());
      assertEquals("r1: " + r1 + " r2: " + r2, r1.fitsInDouble(), r2.fitsInDouble());
      assertEquals("r1: " + r1 + " r2: " + r2, r1.fitsInBigInteger(), r2.fitsInBigInteger());

      if (r1.fitsInLong()) {
        assertEquals("Results for " + n1 + operation + " " + n2, r1.asLong(), r2.asLong());
      } else if (r1.fitsInDouble()) {
        assertEquals("Results for " + n1 + operation + " " + n2, r1.asDouble(), r2.asDouble(), 0.1);
      } else if (r1.fitsInBigInteger()) {
        assertEquals("Results for " + n1 + operation + " " + n2, r1.asBigInteger(), r2.asBigInteger());
      } else if (r1.isBoolean()) {
        assertEquals("Results for " + n1 + operation + " " + n2, r1.asBoolean(), r2.asBoolean());
      } else {
        fail("Doesn't fit: " + r1);
      }

      return null;
    });
  }
}
