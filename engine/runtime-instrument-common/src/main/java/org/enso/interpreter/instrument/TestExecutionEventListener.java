package org.enso.interpreter.instrument;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventListener;
import com.oracle.truffle.api.instrumentation.SourceFilter;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.instrumentation.TruffleInstrument.Registration;
import java.io.PrintStream;
import org.apache.commons.io.output.NullOutputStream;

@Registration(id = TestExecutionEventListener.ID, services = TestExecutionEventListener.class)
public class TestExecutionEventListener extends TruffleInstrument
    implements ExecutionEventListener {
  private static final PrintStream LOG = new PrintStream(NullOutputStream.INSTANCE); // System.err
  public static final String ID = "test-execution-event-listener";

  @Override
  protected void onCreate(Env env) {
    env.registerService(this);
    var jsSrcFilter = SourceFilter.newBuilder().languageIs("js").build();
    var sourceSectionFilter = SourceSectionFilter.newBuilder().build();
    env.getInstrumenter().attachExecutionEventListener(SourceSectionFilter.ANY, this);
  }

  @Override
  public void onEnter(EventContext context, VirtualFrame frame) {
    LOG.printf("onEnter: %s%n", context);
  }

  @Override
  public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {
    LOG.printf("onReturnValue: ctx=%s, result=%s %n", context, result);
  }

  @Override
  public void onReturnExceptional(EventContext context, VirtualFrame frame, Throwable exception) {
    LOG.printf("onReturnExceptional: ctx=%s, exception=%s %n", context, exception);
    exception.printStackTrace(LOG);
    LOG.println();
  }
}
