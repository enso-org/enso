package org.enso.interpreter.instrument;

import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;

import java.util.function.Consumer;

/**
 * A simple instrument scaffold for instruments using the {@link ExactPositionListener} through a
 * {@link Consumer} callback.
 *
 * @param <T> the type of values this instrument calls the consumer with.
 */
public abstract class ExactPositionInstrument<T> extends TruffleInstrument {
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
   * Creates an instance of the listener.
   *
   * @param funName the function name the listener should trigger for.
   * @param sourceStart the source start of the instrumented location.
   * @param length the length of the instrumented location.
   * @param callback the callback passed by the user to trigger when the location is instrumented.
   * @return an instance of {@link ExactPositionListener}.
   */
  public abstract ExactPositionListener createListener(
      String funName, int sourceStart, int length, Consumer<T> callback);

  /**
   * Creates a source section filter for the instrument.
   *
   * @param funName the function name this listener should trigger for.
   * @param sourceStart the source start of the instrumented location.
   * @param length the length of the instrumented location.
   * @return the source section filter corresponding to the parameters.
   */
  public abstract SourceSectionFilter createSourceSectionFilter(
      String funName, int sourceStart, int length);

  /**
   * Attach a new listener to observe nodes with given parameters.
   *
   * @param funName the function name the listener should trigger for.
   * @param sourceStart the source start location of the expected node.
   * @param length the source length of the expected node.
   * @param callback the consumer of the node value.
   * @return a reference to attached event listener.
   */
  public EventBinding<ExactPositionListener> bindTo(
      String funName, int sourceStart, int length, Consumer<T> callback) {
    ExactPositionListener listener = createListener(funName, sourceStart, length, callback);
    SourceSectionFilter filter = createSourceSectionFilter(funName, sourceStart, length);

    EventBinding<ExactPositionListener> binding =
        env.getInstrumenter().attachExecutionEventListener(filter, listener);
    listener.setBinding(binding);
    return binding;
  }
}
