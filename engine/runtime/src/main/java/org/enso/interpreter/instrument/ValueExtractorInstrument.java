package org.enso.interpreter.instrument;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;

import java.util.concurrent.Callable;
import java.util.function.Consumer;

/**
 * An instrument used to extract node values from currently executed functions.
 *
 * <p>Allows to listen for a node at a given position, and trigger a callback when the node is
 * executed for the first time, passing the node's return value to the callback.
 */
@TruffleInstrument.Registration(
    id = ValueExtractorInstrument.INSTRUMENT_ID,
    services = ValueExtractorInstrument.class)
public class ValueExtractorInstrument extends ExactPositionInstrument<Object> {
  public static final String INSTRUMENT_ID = "value-extractor";

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
      String funName, int sourceStart, int length, Consumer<Object> callback) {
    return new ExactPositionListener(funName, sourceStart, length) {
      @Override
      public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {
        if (!shouldTrigger(context)) {
          return;
        }
        detach();
        callback.accept(result);
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
        .tagIs(StandardTags.ExpressionTag.class)
        .indexIn(sourceStart, length)
        .build();
  }
}
