package org.enso.interpreter.test.asserts;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.ClassRule;
import org.junit.Test;

public class DisabledAssertionsTest {
  @ClassRule
  public static final ContextUtils ctxRule =
      ContextUtils.newBuilder()
          .withModifiedContext(b -> b.environment("ENSO_ENABLE_ASSERTIONS", "false"))
          .build();

  @Test
  public void assertionsCanBeDisabledWithEnvVar() {
    var ensoCtx = ctxRule.ensoContext();
    assertFalse(ensoCtx.isAssertionsEnabled());
  }

  @Test
  public void actionInAssertIsNotComputedWhenAssertionsAreDisabled() {
    Value res =
        ctxRule.evalModule(
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
