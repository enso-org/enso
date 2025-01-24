package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.concurrent.Executors;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class ForeignMethodInvokeTest {
  private static Context ctx;

  @BeforeClass
  public static void prepareCtx() {
    ctx = ContextUtils.defaultContextBuilder("enso", "js").build();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
    ctx = null;
  }

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
    Value module = ctx.eval(src);
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
          lines[0].matches("Cannot parse.*foreign python.*method.*languages are .+"));
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

    var module = ctx.eval("enso", source);
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

  @Ignore
  @Test
  public void testParallelInteropWithJavaScript() throws Exception {
    var source =
        """
        from Standard.Base import all

        polyglot java import java.lang.Thread

        foreign js js_array t f = \"\"\"
            f(300)
            return [1, 2, t]

        third t = js_array t (delay-> Thread.sleep delay)
        """;

    var module = ctx.eval("enso", source);
    var third = module.invokeMember("eval_expression", new AsString("third"));

    var future =
        Executors.newSingleThreadExecutor()
            .submit(
                () -> {
                  return third.execute(12);
                });
    var res = third.execute(13);
    assertTrue("It is an array", res.hasArrayElements());
    assertEquals(3, res.getArraySize());
    assertEquals(1, res.getArrayElement(0).asInt());
    assertEquals(2, res.getArrayElement(1).asInt());
    assertEquals(13, res.getArrayElement(2).asInt());

    var res2 = future.get();

    assertTrue("It is an array2", res2.hasArrayElements());
    assertEquals(12, res2.getArrayElement(2).asInt());
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
