package org.enso.interpreter.instrument;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;

@TruffleInstrument.Registration(
    id = ValueOverrideInstrument.INSTRUMENT_ID,
    services = ValueOverrideInstrument.class)
public class ValueOverrideInstrument extends TruffleInstrument {
  public static final String INSTRUMENT_ID = "value-override";

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

  /** The Listener handling this instrument's logic. */
  public static class Listener extends ExactPositionListener {
    private final Object value;

    private Listener(String funName, int start, int length, Object value) {
      super(funName, start, length);
      this.value = value;
    }

    /**
     * Triggered when the node would execute. Throws itself an unwind containing the override value
     * instead.
     *
     * @param context the event context.
     * @param frame the current execution frame.
     */
    @Override
    public void onEnter(EventContext context, VirtualFrame frame) {
      if (shouldTrigger(context)) {
        throw context.createUnwind(value, getBinding());
      }
    }

    /**
     * Catches the unwind it's thrown itself from {@link #onEnter(EventContext, VirtualFrame)} and
     * returns it as the value of the executed node.
     *
     * @param context the event context.
     * @param frame the current execution frame.
     * @param info the object containing the override return value.
     * @return the {@code info} argument.
     */
    @Override
    public Object onUnwind(EventContext context, VirtualFrame frame, Object info) {
      return info;
    }
  }

  /**
   * Attaches the instrument at a requested source location.
   *
   * @param funName the function name this instrument should trigger inside.
   * @param sourceStart the source start of the overridden node.
   * @param sourceLength the source length of the overridden node.
   * @param value the value to use as the override.
   * @return the event binding for the specified parameters.
   */
  public EventBinding<Listener> overrideAt(
      String funName, int sourceStart, int sourceLength, Object value) {
    SourceSectionFilter sourceSectionFilter =
        SourceSectionFilter.newBuilder()
            .indexIn(sourceStart, sourceLength)
            .tagIs(StandardTags.ExpressionTag.class)
            .build();
    Listener listener = new Listener(funName, sourceStart, sourceLength, value);

    EventBinding<Listener> binding =
        env.getInstrumenter().attachExecutionEventListener(sourceSectionFilter, listener);
    listener.setBinding(binding);
    return binding;
  }
}
