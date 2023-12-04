package org.enso.interpreter.instrument;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventListener;
import com.oracle.truffle.api.instrumentation.SourceFilter;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.instrumentation.TruffleInstrument.Registration;

@Registration(
    id = TestExecutionEventListener.ID,
    services = TestExecutionEventListener.class
)
public class TestExecutionEventListener extends TruffleInstrument implements
    ExecutionEventListener {

  public static final String ID = "test-execution-event-listener";

  @Override
  protected void onCreate(Env env) {
    env.registerService(this);
    var jsSrcFilter = SourceFilter.newBuilder().languageIs("js").build();
    var sourceSectionFilter = SourceSectionFilter.newBuilder()
        .build();
    env.getInstrumenter().attachExecutionEventListener(SourceSectionFilter.ANY, this);
  }

  @Override
  public void onEnter(EventContext context, VirtualFrame frame) {
    System.out.printf("onEnter: %s%n", context);
  }

  @Override
  public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {
    System.out.printf("onReturnValue: ctx=%s, result=%s %n", context, result);
  }

  @Override
  public void onReturnExceptional(EventContext context, VirtualFrame frame, Throwable exception) {
    System.out.printf("onReturnExceptional: ctx=%s, exception=%s %n", context, exception);
    System.out.println();
  }
}
