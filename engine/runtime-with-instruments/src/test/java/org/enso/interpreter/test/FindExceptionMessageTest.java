package org.enso.interpreter.test;

import org.enso.interpreter.instrument.job.VisualizationResult;
import org.enso.polyglot.HostEnsoUtils;
import static org.junit.Assert.assertEquals;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
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
    String src = """
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
    String src = """
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
  public void testPanic() {
    String src = """
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

    executeInContext(ctx, () -> {
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
