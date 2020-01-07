package org.enso.interpreter.instrument;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

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
public class ValueExtractorInstrument extends TruffleInstrument {
  public static final String INSTRUMENT_ID = "value-extractor";
  private Env env;

  /**
   * Initializes the instrument. Substitute for a constructor, called by the Truffle framework.
   *
   * @param env the instrumentation environment
   */
  @Override
  protected void onCreate(Env env) {
    env.registerService(this);
    this.env = env;
  }

  /**
   * An event listener implementing the behavior of verifying whether the currently executed node is
   * the one expected by the user and passing the computed value to the callback.
   */
  public static class ValueEventListener implements ExecutionEventListener {
    private EventBinding<ValueEventListener> binding;
    private final Consumer<Object> callback;
    private final int start;
    private final int length;

    private ValueEventListener(int start, int length, Consumer<Object> callback) {
      this.start = start;
      this.length = length;
      this.callback = callback;
    }

    private void setBinding(EventBinding<ValueEventListener> binding) {
      this.binding = binding;
    }

    /**
     * Get the start location of the nodes expected by this listener.
     *
     * @return the start location for this listener
     */
    public int getStart() {
      return start;
    }

    /**
     * Get the source length of the nodes expected by this listener.
     *
     * @return the source length for this listener
     */
    public int getLength() {
      return length;
    }

    /**
     * Was a node with parameters specified for this listener encountered in the course of
     * execution?
     *
     * @return {@code true} if the requested node was observed, {@code false} otherwise
     */
    public boolean isSuccessful() {
      return binding.isDisposed();
    }

    @Override
    public void onEnter(EventContext context, VirtualFrame frame) {}

    /**
     * Checks if the node to be executed is the node this listener was created to observe and
     * triggers the callback if the correct node just finished executing.
     *
     * @param context current execution context
     * @param frame current execution frame
     * @param result the return value of the currently executed node
     */
    @Override
    public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {
      Node node = context.getInstrumentedNode();
      SourceSection section = node.getSourceSection();
      if (section == null || !section.hasCharIndex()) {
        return;
      }
      if (section.getCharIndex() == start && section.getCharLength() == length) {
        binding.dispose();
        callback.accept(result);
      }
    }

    @Override
    public void onReturnExceptional(
        EventContext context, VirtualFrame frame, Throwable exception) {}
  }

  /**
   * Attach a new listener to observe nodes with given parameters.
   *
   * @param sourceStart the source start location of the expected node
   * @param length the source length of the expected node
   * @param callback the consumer of the node value
   * @return a reference to attached event listener
   */
  public EventBinding<ValueEventListener> bindTo(
      int sourceStart, int length, Consumer<Object> callback) {
    ValueEventListener listener = new ValueEventListener(sourceStart, length, callback);

    EventBinding<ValueEventListener> binding =
        env.getInstrumenter()
            .attachExecutionEventListener(
                SourceSectionFilter.newBuilder().indexIn(sourceStart, length).build(), listener);
    listener.setBinding(binding);
    return binding;
  }
}
