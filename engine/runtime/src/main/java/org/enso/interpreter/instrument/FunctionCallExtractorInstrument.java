package org.enso.interpreter.instrument;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import java.util.function.Consumer;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;

/** An instrument used to extract function call information. */
@TruffleInstrument.Registration(
    id = FunctionCallExtractorInstrument.INSTRUMENT_ID,
    services = FunctionCallExtractorInstrument.class)
public class FunctionCallExtractorInstrument
    extends ExactPositionInstrument<FunctionCallInstrumentationNode.FunctionCall> {
  public static final String INSTRUMENT_ID = "function-call-extractor";

  /**
   * Creates the listener instance for this instrument and given source info.
   *
   * @param funName the function name the listener should trigger for.
   * @param sourceStart the source start of the instrumented location.
   * @param length the length of the instrumented location.
   * @param callback the callback passed by the user to trigger when the location is instrumented.
   * @return the listener instance.
   */
  @Override
  public ExactPositionListener createListener(
      String funName,
      int sourceStart,
      int length,
      Consumer<FunctionCallInstrumentationNode.FunctionCall> callback) {
    return new ExactPositionListener(funName, sourceStart, length) {
      @Override
      public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {
        if (!shouldTrigger(context)) {
          return;
        }
        if (result instanceof FunctionCallInstrumentationNode.FunctionCall) {
          detach();
          callback.accept((FunctionCallInstrumentationNode.FunctionCall) result);
        }
      }
    };
  }

  /**
   * Creates the source section filter for this instrument.
   *
   * @param funName the function name this listener should trigger for.
   * @param sourceStart the source start of the instrumented location.
   * @param length the length of the instrumented location.
   * @return the source section filter.
   */
  @Override
  public SourceSectionFilter createSourceSectionFilter(
      String funName, int sourceStart, int length) {
    return SourceSectionFilter.newBuilder()
        .tagIs(StandardTags.CallTag.class)
        .indexIn(sourceStart, length)
        .build();
  }
}
