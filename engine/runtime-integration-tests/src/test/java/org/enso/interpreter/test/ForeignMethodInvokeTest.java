package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.concurrent.Callable;
import java.util.concurrent.Executors;
import org.enso.common.MethodNames;
import org.enso.runtime.utils.ThreadUtils;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.junit.ClassRule;
import org.junit.Test;

public class ForeignMethodInvokeTest {
  @ClassRule
  public static final ContextUtils ctxRule =
      ContextUtils.newBuilder("enso", "js").alwaysExecuteInContext(false).build();

  @Test
  public void testForeignFunctionParseFailure() throws Exception {
    // python is not a permitted language, therefore, invoking `py_array` method
    // should fail with a Polyglot_Error, rather than crashing whole engine.
    var code =
        """
        from Standard.Base import all

        foreign python py_array = \"\"\"
            return [1,2,3]

        main =
            Panic.recover Any py_array
        """
            .trim();
    var src = Source.newBuilder("enso", code, "TryPython.enso").build();
    Value module = ctxRule.eval(src);
    Value res = module.invokeMember("eval_expression", "main");
    assertTrue("Invoking non-installed foreign function should recover", res.isException());
    try {
      throw res.throwException();
    } catch (RuntimeException e) {
      var sw = new StringWriter();
      var pw = new PrintWriter(sw);
      e.printStackTrace(pw);
      var text = sw.toString().replace(System.getProperty("line.separator"), "\n");
      var lines = text.split("\n");
      assertThat(
          "Expecting message at first line: " + lines[0],
          lines[0].matches(".*Cannot parse.*foreign python.*method.*languages are .+"));
      assertThat(
          "First error line comes from TryPython file: " + lines[1],
          lines[1].matches(".*at <enso> TryPython\\.py_array\\(TryPython:3.*\\)"));
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

    var module = ctxRule.eval("enso", source);
    var third = module.invokeMember("eval_expression", new AsString("third"));
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

  private static final long TIMEOUT = 30000;

  @Test(timeout = TIMEOUT)
  public void testParallelInteropWithJavaScript() throws Exception {
    var orig = Thread.currentThread();
    var pool = Executors.newSingleThreadExecutor();
    var watchDog =
        pool.submit(
            () -> {
              Thread.sleep(TIMEOUT / 3 * 2);
              var dump =
                  ThreadUtils.dumpAllStacktraces(
                      "[paralleljs] ", "Time out in a testParallelInteropWithJavaScript");
              System.err.println(dump);
              orig.interrupt();
              return dump;
            });
    try {
      handleParallelInteropWithJavaScript();
    } finally {
      watchDog.cancel(false);
      pool.shutdown();
    }
  }

  private void handleParallelInteropWithJavaScript() throws Exception {
    var source =
        """
        from Standard.Base import all

        polyglot java import java.lang.Thread

        foreign js js_array t f = \"\"\"
            f(300)
            return [1, 2, t]

        third t = js_array t (delay-> Thread.sleep delay)
        """;

    var module = ctxRule.eval("enso", source);
    var third = module.invokeMember("eval_expression", new AsString("third"));

    var pool =
        Executors.newSingleThreadExecutor(
            (r) -> {
              return new Thread(r, "testParallelInteropWithJavaScript 2nd");
            });

    // action12 and action13 will be invoke in parallel...
    Callable<Value> action12 =
        () -> {
          return third.execute(12);
        };
    Callable<Value> action13 =
        () -> {
          return third.execute(13);
        };
    try {
      var futureResult12 = pool.submit(action12);
      var result13 = action13.call();

      assertTrue("It is an array", result13.hasArrayElements());
      assertEquals(3, result13.getArraySize());
      assertEquals(1, result13.getArrayElement(0).asInt());
      assertEquals(2, result13.getArrayElement(1).asInt());
      assertEquals(13, result13.getArrayElement(2).asInt());

      var result12 = futureResult12.get();

      assertTrue("It is an array2", result12.hasArrayElements());
      assertEquals(12, result12.getArrayElement(2).asInt());
    } finally {
      pool.shutdownNow();
    }
  }

  @Test
  public void testParallelAccessToState() throws Exception {
    var source =
        """
        from Standard.Base import all
        import Standard.Base.Runtime.State

        polyglot java import org.enso.example.TestClass

        type Data
            read times = times * State.get Data
            run v ~action = State.run Data v action

        callme f =
            Data.run 6 <|
                f Data.read 7
        """;

    var module = ctxRule.eval("enso", source);
    var callme = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "callme");
    var readData = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Data.read");

    var middleAction = new Runnable[1];
    var callback =
        (ProxyExecutable)
            (Value... arguments) -> {
              assertTrue("Got an executable", arguments[0].canExecute());
              if (middleAction[0] != null) {
                var r = middleAction[0];
                middleAction[0] = null;
                r.run();
              }
              return arguments[0].execute(arguments[1]);
            };

    var middleActionResult = new Value[1];
    middleAction[0] =
        () -> {
          var second =
              new Thread("Access in second thread") {
                @Override
                public void run() {
                  try {
                    var value = readData.execute(2);
                    fail("No value should be returned: " + value);
                  } catch (PolyglotException ex) {
                    middleActionResult[0] = ex.getGuestObject();
                  }
                }
              };
          second.start();
          try {
            second.join();
          } catch (InterruptedException e) {
            throw new AssertionError(e);
          }
        };

    var result = callme.execute(callback);
    assertEquals("State in first thread is set to 6*7", 42, result.asInt());
    assertNotNull("State in second thread is not set to anything", middleActionResult[0]);
    assertEquals("Panic", middleActionResult[0].getMetaObject().getMetaSimpleName());
    assertEquals("(Uninitialized_State.Error Data)", middleActionResult[0].toString());
  }

  @ExportLibrary(InteropLibrary.class)
  static class AsString implements TruffleObject {
    private final String value;

    private AsString(String value) {
      this.value = value;
    }

    @ExportMessage
    boolean isString() {
      return true;
    }

    @ExportMessage
    String asString() {
      return value;
    }
  }
}
