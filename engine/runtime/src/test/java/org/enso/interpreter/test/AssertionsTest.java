package org.enso.interpreter.test;

import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class AssertionsTest extends TestBase {
  private static Context ctx;

  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();


  @BeforeClass
  public static void setupCtx() {
    ctx = TestBase.defaultContextBuilder(LanguageInfo.ID)
        .option(RuntimeOptions.ENABLE_ASSERTIONS, "true")
        .out(out)
        .err(out)
        .build();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close(true);
  }

  @After
  public void resetOutput() {
    out.reset();
  }

  @Test
  public void simpleAssertionFailure() {
    try {
      TestBase.evalModule(ctx, """
      from Standard.Base import False, Runtime
      main = Runtime.assert False
      """);
      fail("Should throw Assertion_Error");
    } catch (PolyglotException e) {
      // TODO: Investigate the exception
      throw new RuntimeException(e);
    }
  }
}
