package org.enso.interpreter.test.interop;

import org.enso.test.utils.ContextUtils;
import org.junit.ClassRule;

public final class HostJavaInteropTest extends JavaInteropTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.newBuilder().build();

  @Override
  protected final ContextUtils ctx() {
    return ctxRule;
  }
}
