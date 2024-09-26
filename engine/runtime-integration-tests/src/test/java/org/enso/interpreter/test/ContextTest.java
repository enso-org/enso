package org.enso.interpreter.test;

import java.io.OutputStream;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;

public class ContextTest {

  protected static Context ctx;

  @BeforeClass
  public static void prepareCtx() {
    ctx =
        ContextUtils.defaultContextBuilder()
            .out(OutputStream.nullOutputStream())
            .err(OutputStream.nullOutputStream())
            .build();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
    ctx = null;
  }
}
