package org.enso.interpreter.test.asserts;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames.TopScope;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class DisabledAssertionsTest {
  private static Context ctx;

  @BeforeClass
  public static void setupCtx() {
    ctx =
        ContextUtils.defaultContextBuilder().environment("ENSO_ENABLE_ASSERTIONS", "false").build();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close(true);
    ctx = null;
  }

  @Test
  public void assertionsCanBeDisabledWithEnvVar() {
    EnsoContext ensoCtx =
        ctx.getBindings(LanguageInfo.ID).invokeMember(TopScope.LEAK_CONTEXT).asHostObject();
    assertFalse(ensoCtx.isAssertionsEnabled());
  }

  @Test
  public void actionInAssertIsNotComputedWhenAssertionsAreDisabled() {
    Value res =
        ContextUtils.evalModule(
            ctx,
            """
from Standard.Base import Runtime
import Standard.Base.Runtime.Ref.Ref

main =
    ref = Ref.new 10
    Runtime.assert (ref.put 23)
    ref.get
""");
    assertTrue(res.isNumber());
    assertThat(res.asInt(), is(10));
  }
}
