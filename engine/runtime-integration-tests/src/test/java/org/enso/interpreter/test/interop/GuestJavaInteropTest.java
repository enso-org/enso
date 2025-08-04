package org.enso.interpreter.test.interop;

import org.enso.test.utils.ContextUtils;
import org.enso.testkit.ReportLogsOnFailureRule;
import org.junit.ClassRule;
import org.junit.Rule;

public final class GuestJavaInteropTest extends JavaInteropTest {
  @ClassRule
  public static final ContextUtils ctxRule =
      ContextUtils.newBuilder()
          .withModifiedContext((b) -> b.option("enso.classLoading", "guest"))
          .build();

  @Rule(order = Integer.MIN_VALUE)
  public ReportLogsOnFailureRule appenderRule = new ReportLogsOnFailureRule();

  @Override
  protected final ContextUtils ctx() {
    return ctxRule;
  }
}
