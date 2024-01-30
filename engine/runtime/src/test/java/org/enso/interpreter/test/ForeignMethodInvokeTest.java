package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Random;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class ForeignMethodInvokeTest extends TestBase {
  private static Context ctx;

  @BeforeClass
  public static void prepareCtx() {
    var r = new Random();
    var counter = new long[1];
    var b =
        defaultContextBuilder("enso", "js")
            .onDeniedThreadAccess(
                (ex) -> {
                  System.err.println(
                      "Denied thread access: "
                          + ex.getMessage()
                          + " in "
                          + Thread.currentThread().getName());
                  if (counter[0] > 500) {
                    throw ex;
                  }
                  try {
                    long waitTime = r.nextInt(100);
                    System.err.println("Waiting " + waitTime + " ms like Ethernet");
                    Thread.sleep(waitTime);
                    counter[0] += waitTime;
                  } catch (InterruptedException ignore) {
                  }
                  System.err.println("Trying again");
                });
    ctx = b.build();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
  }

  @Test
  public void testForeignFunctionParseFailure() {
    // python is not a permitted language, therefore, invoking `py_array` method
    // should fail with a Polyglot_Error, rather than crashing whole engine.
    String source =
        """
        from Standard.Base import all

        foreign python py_array = \"\"\"
            return [1,2,3]

        main =
            Panic.recover Any py_array
        """
            .trim();
    Value module = ctx.eval("enso", source);
    Value res = module.invokeMember("eval_expression", "main");
    assertTrue("Invoking non-installed foreign function should recover", res.isException());
    try {
      throw res.throwException();
    } catch (Exception e) {
      assertTrue(
          "Wrong error message",
          e.getMessage()
              .matches("Cannot parse foreign python method. Only available languages are .+"));
    }
  }

  @Test
  public void testInteropWithJavaScript() throws Exception {
    var source =
        """
        from Standard.Base import all

        foreign js js_array t = \"\"\"
            return [1, 2, t]

        third t = js_array t
        """;

    var module = ctx.eval("enso", source);
    var third = module.invokeMember("eval_expression", "third");
    var res = third.execute(13);
    assertTrue("It is an array", res.hasArrayElements());
    assertEquals(3, res.getArraySize());
    assertEquals(1, res.getArrayElement(0).asInt());
    assertEquals(2, res.getArrayElement(1).asInt());
    assertEquals(13, res.getArrayElement(2).asInt());

    var res2 =
        Executors.newSingleThreadExecutor()
            .submit(
                () -> {
                  return third.execute(12);
                })
            .get();

    assertTrue("It is an array2", res2.hasArrayElements());
    assertEquals(12, res2.getArrayElement(2).asInt());
  }

  @Test
  public void testParallelInteropWithJavaScript() throws Exception {
    var source =
        """
        from Standard.Base import all

        polyglot java import java.lang.Thread

        foreign js js_array t f = \"\"\"
            print("In JavaScript...")
            f(300)
            print("In JavaScript after waiting...")
            return [1, 2, t]

        third t = js_array t (delay-> Thread.sleep delay)
        """;

    var module = ctx.eval("enso", source);
    var third = module.invokeMember("eval_expression", "third");

    var future =
        Executors.newSingleThreadExecutor()
            .submit(
                () -> {
                  try {
                    Thread.sleep(100);
                    System.err.println(
                        "Entering context in executor thread in "
                            + Thread.currentThread().getName());
                    ctx.enter();
                    System.err.println("Entered context in executor thread");
                    return third.execute(12);
                  } catch (InterruptedException ex) {
                    ex.printStackTrace();
                    return null;
                  } finally {
                    ctx.leave();
                    System.err.println("Left context in executor thread");
                  }
                });
    ctx.enter();
    System.err.println("Entered context on main thread in " + Thread.currentThread().getName());
    var res = third.execute(13);
    System.err.println("Got result on main thread: " + res);
    assertTrue("It is an array", res.hasArrayElements());
    assertEquals(3, res.getArraySize());
    assertEquals(1, res.getArrayElement(0).asInt());
    assertEquals(2, res.getArrayElement(1).asInt());
    assertEquals(13, res.getArrayElement(2).asInt());
    ctx.leave();
    System.err.println("Context left on main thread");

    var res2 = future.get(10, TimeUnit.SECONDS);

    System.err.println("Future result obtained: " + res2);

    assertTrue("It is an array2", res2.hasArrayElements());
    assertEquals(12, res2.getArrayElement(2).asInt());
  }
}
