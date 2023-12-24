package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.enso.interpreter.instrument.job.VisualizationResult;
import org.enso.polyglot.HostEnsoUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class FindExceptionMessageTest extends TestBase {

  private static Context ctx;

  @BeforeClass
  public static void initCtx() {
    ctx = createDefaultContext();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
  }

  @Test
  public void testThrowNPE() {
    String src =
        """
    from Standard.Base import Panic
    polyglot java import java.lang.NullPointerException

    main =
        x = NullPointerException.new
        Panic.throw x
    """;

    try {
      Value res = evalModule(ctx, src);
      fail("No result expected: " + res);
    } catch (PolyglotException ex) {
      assertExceptionMessage("java.lang.NullPointerException", ex);
    }
  }

  @Test
  public void testThrowNPEWithName() {
    String src =
        """
    from Standard.Base import Panic
    polyglot java import java.lang.NullPointerException

    main =
        x = NullPointerException.new "Hello World!"
        Panic.throw x
    """;

    try {
      Value res = evalModule(ctx, src);
      fail("No result expected: " + res);
    } catch (PolyglotException ex) {
      assertExceptionMessage("Hello World!", ex);
    }
  }

  @Test
  public void errorThrowDeep() {
    var src =
        """
    from Standard.Base import all
    import Standard.Base.Errors.Illegal_Argument.Illegal_Argument

    deep n = if n <= 0 then Error.throw (Illegal_Argument.Error "Problem"+n.to_text) else deep n-1

    main =
        d = deep 10
        d
    """;

    var res = evalModule(ctx, src);
    assertTrue("Expecting error: " + res, res.isException());
    assertEquals("Standard.Base.Error.Error", res.getMetaObject().getMetaQualifiedName());

    try {
      throw res.throwException();
    } catch (PolyglotException ex) {
      assertNotNull("Has source location", ex.getSourceLocation());
      var throwCode = ex.getSourceLocation().getCharacters().toString();
      assertNotEquals(
          "Throw code found in the source code: " + throwCode, -1, src.indexOf(throwCode));
    }
  }

  @Test
  public void panicThrowDeepRecoverError() {
    var src =
        """
    from Standard.Base import all
    import Standard.Base.Errors.Illegal_Argument.Illegal_Argument

    deep_panic n = if n <= 0 then Panic.throw (Illegal_Argument.Error "Problem") else
        deep_panic n-1

    main =
        d = Panic.recover Any
            deep_panic 10
        d
    """;

    var res = evalModule(ctx, src);
    assertTrue("Expecting recovered error: " + res, res.isException());
    assertEquals(
        "Panic was converted to error",
        "Standard.Base.Error.Error",
        res.getMetaObject().getMetaQualifiedName());

    try {
      throw res.throwException();
    } catch (PolyglotException ex) {
      assertNull("No source location...", ex.getSourceLocation());
      assertNotNull("... but stacktrace is fine", ex.getPolyglotStackTrace());
      var cnt = 0;
      for (var f : ex.getPolyglotStackTrace()) {
        if ("Unnamed.deep_panic".equals(f.getRootName())) {
          cnt++;
        }
      }
      assertEquals("Contains proper amount of deep_panic invocations", 11, cnt);
    }
  }

  @Test
  public void panicThrowDeepMixingJava() {
    var src =
        """
    from Standard.Base import all
    import Standard.Base.Errors.Illegal_Argument.Illegal_Argument
    polyglot java import org.enso.example.TestClass

    exec e ~r =
        e.execute r...

    deep_panic e n = if n <= 0 then Panic.throw (Illegal_Argument.Error "Problem") else
        exec e
            deep_panic e n-1

    main =
        e = TestClass.newDirectExecutor
        d = Panic.recover Any
            deep_panic e 10
        d
    """;

    var res = evalModule(ctx, src);
    assertTrue("Expecting recovered error: " + res, res.isException());
    assertEquals(
        "Panic was converted to error",
        "Standard.Base.Error.Error",
        res.getMetaObject().getMetaQualifiedName());

    try {
      throw res.throwException();
    } catch (PolyglotException ex) {
      assertNull("No source location...", ex.getSourceLocation());
      assertNotNull("... but stacktrace is fine", ex.getPolyglotStackTrace());
      var countDeepPanic = 0;
      var countTestClass = 0;
      for (var f : ex.getPolyglotStackTrace()) {
        var rootName = f.getRootName();
        if ("Unnamed.deep_panic".equals(rootName)) {
          countDeepPanic++;
        }
        if (rootName.contains("TestClass") && rootName.equals("newDirectExecutor")) {
          countTestClass++;
        }
      }
      assertEquals("Contains proper amount of deep_panic invocations", 11, countDeepPanic);
    }
  }

  @Test
  public void testPanic() {
    String src =
        """
    from Standard.Base import Panic
    import Standard.Base.Errors.Illegal_Argument.Illegal_Argument

    main = Panic.throw (Illegal_Argument.Error 'Jejda!')

    """;

    try {
      Value res = evalModule(ctx, src);
      fail("No result expected: " + res);
    } catch (PolyglotException ex) {
      assertExceptionMessage("Illegal Argument: Jejda!", ex);
    }
  }

  private void assertExceptionMessage(String exp, PolyglotException ex) {
    var msg = HostEnsoUtils.findExceptionMessage(ex);
    assertEquals(exp, msg);

    executeInContext(
        ctx,
        () -> {
          var guestException = extractHostException(ex);
          var guestMsg = VisualizationResult.findExceptionMessage(guestException);
          assertEquals(exp, guestMsg);
          return null;
        });
  }

  static Throwable extractHostException(PolyglotException ex) {
    if (ex.isHostException()) {
      return ex.asHostException();
    } else {
      assertTrue("Has to be guest object: " + ex, ex.isGuestException());
      var v = ex.getGuestObject();
      return (Throwable) unwrapValue(ctx, v);
    }
  }
}
