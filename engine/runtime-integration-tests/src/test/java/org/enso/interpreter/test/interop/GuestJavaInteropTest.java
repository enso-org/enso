package org.enso.interpreter.test.interop;

import org.enso.test.utils.ContextUtils;
import org.junit.ClassRule;

public final class GuestJavaInteropTest extends JavaInteropTest {
  @ClassRule
  public static final ContextUtils ctxRule =
      ContextUtils.newBuilder()
          .withModifiedContext((b) -> b.option("enso.classLoading", "guest"))
          .build();

  @Override
  protected final ContextUtils ctx() {
    return ctxRule;
  }
}
