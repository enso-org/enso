package org.enso.interpreter.test.instruments;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventListener;
import com.oracle.truffle.api.instrumentation.Instrumenter;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import java.io.PrintWriter;
import java.util.function.Consumer;
import java.util.function.Predicate;

@TruffleInstrument.Registration(id = VariablesTestInstrument.ID, services = Consumer.class)
public final class VariablesTestInstrument extends TruffleInstrument
    implements Consumer<PrintWriter> {
  public static final String ID = "varibles-read-write-test";

  private Instrumenter instr;
  private EventBinding<VariablesTestInstrument.TraceExecution> close1;
  private EventBinding<VariablesTestInstrument.TraceExecution> close2;

  @Override
  protected void onCreate(TruffleInstrument.Env env) {
    instr = env.getInstrumenter();
    env.registerService(this);
  }

  @Override
  public void accept(PrintWriter pw) {
    if (close1 != null) {
      close1.dispose();
      close2.dispose();
    }
    if (pw != null) {
      var indent = new StringBuilder();
      Predicate<String> nameCheck =
          (n) -> {
            return n.endsWith("main");
          };
      var filterRead =
          SourceSectionFilter.newBuilder()
              .rootNameIs(nameCheck)
              .tagIs(StandardTags.ReadVariableTag.class)
              .build();
      close1 =
          instr.attachExecutionEventListener(
              filterRead, new TraceExecution(StandardTags.ReadVariableTag.NAME, pw, indent));
      var filterWrite =
          SourceSectionFilter.newBuilder()
              .rootNameIs(nameCheck)
              .tagIs(StandardTags.WriteVariableTag.class)
              .build();
      close2 =
          instr.attachExecutionEventListener(
              filterWrite, new TraceExecution(StandardTags.WriteVariableTag.NAME, pw, indent));
    }
  }

  private static final class TraceExecution implements ExecutionEventListener {
    private final String attrName;
    private final PrintWriter output;
    private final StringBuilder indent;

    TraceExecution(String memberName, PrintWriter output, StringBuilder indent) {
      this.attrName = memberName;
      this.output = output;
      this.indent = indent;
    }

    @Override
    public void onEnter(EventContext context, VirtualFrame frame) {
      try {
        var to = context.getNodeObject();
        var iop = InteropLibrary.getUncached();
        Object name = "";
        if (iop.isMemberReadable(to, attrName)) {
          name = iop.readMember(to, attrName);
        }
        output.println(indent + attrName + "Enter " + name);
      } catch (InteropException ex) {
        ex.printStackTrace(output);
      }
      indent.append("  ");
    }

    @Override
    public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {
      indent.delete(0, 2);
      output.println(indent + attrName + "Return = " + result);
      output.flush();
    }

    @Override
    public void onReturnExceptional(EventContext context, VirtualFrame frame, Throwable exception) {
      indent.delete(0, 2);
      output.println(indent + attrName + "ReturnExceptional");
      exception.printStackTrace(output);
    }
  }
}
