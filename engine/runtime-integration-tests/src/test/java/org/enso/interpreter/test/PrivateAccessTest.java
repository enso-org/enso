package org.enso.interpreter.test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class PrivateAccessTest extends TestBase {
  private static Context ctx;

  @BeforeClass
  public static void initContext() {
    ctx = createDefaultContext();
  }

  @AfterClass
  public static void closeContext() {
    ctx.close();
  }

  @Test
  public void privateConstructorIsNotExposedToPolyglot() {
    var src = """
        type My_Type
            private Cons data
        main = My_Type.Cons 42
        """;
    var obj = TestBase.evalModule(ctx, src);
    assertThat(obj.isNumber(), is(false));
  }
}
