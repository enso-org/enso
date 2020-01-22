package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameInstance;
import com.oracle.truffle.api.frame.FrameInstanceVisitor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventListener;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

/**
 * A simple {@link ExecutionEventListener} scaffolding, capable of triggering for an exact code
 * position, properly handling recursive calls (fires only in the top frame).
 */
public class ExactPositionListener implements ExecutionEventListener {
  private EventBinding<? extends ExactPositionListener> binding;
  private final int start;
  private final int length;
  private final String funName;

  /**
   * Creates an instance of this listener.
   *
   * @param funName the function name for this listener to trigger in.
   * @param start the source start of the instrumented location.
   * @param length the source length of the instrumented location.
   */
  public ExactPositionListener(String funName, int start, int length) {
    this.funName = funName;
    this.start = start;
    this.length = length;
  }

  /**
   * Sets the insternally stored event binding for this listener. It should always be called after
   * the listener is attached inside an instrument. This mechanism allows the listener to detach
   * itself after it first triggers, ensuring the at-most-once semantics.
   *
   * @param binding the event binding resulting from attaching this listener.
   */
  public void setBinding(EventBinding<? extends ExactPositionListener> binding) {
    this.binding = binding;
  }

  /**
   * Checks if we're not inside a recursive call, i.e. the {@link #funName} only appears in the
   * stack trace once.
   *
   * @return {@code true} if it's not a recursive call, {@code false} otherwise.
   */
  private boolean isTopFrame() {
    Object result =
        Truffle.getRuntime()
            .iterateFrames(
                new FrameInstanceVisitor<Object>() {
                  boolean seenFirst = false;

                  @Override
                  public Object visitFrame(FrameInstance frameInstance) {
                    CallTarget ct = frameInstance.getCallTarget();
                    if (ct instanceof RootCallTarget
                        && !funName.equals(((RootCallTarget) ct).getRootNode().getName())) {
                      return null;
                    }
                    if (seenFirst) {
                      return new Object();
                    } else {
                      seenFirst = true;
                      return null;
                    }
                  }
                });
    return result == null;
  }

  /**
   * Checks whether the listener should trigger in the current context.
   *
   * <p>The conditions checked are:
   * <li>Is it not a recursive call?
   * <li>Is the node at the exact requested source position?
   *
   * @param context the current event context.
   * @return true if the listener should trigger, false otherwise.
   */
  protected boolean shouldTrigger(EventContext context) {
    if (!isTopFrame()) {
      return false;
    }
    Node node = context.getInstrumentedNode();
    SourceSection section = node.getSourceSection();
    if (section == null || !section.hasCharIndex()) {
      return false;
    }
    return section.getCharIndex() == start && section.getCharLength() == length;
  }

  /** Detach this listener, ensuring it won't ever trigger. */
  protected void detach() {
    binding.dispose();
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
   * Was a node with parameters specified for this listener encountered in the course of execution?
   *
   * @return {@code true} if the requested node was observed, {@code false} otherwise
   */
  public boolean isSuccessful() {
    return binding.isDisposed();
  }

  /**
   * Get the current binding associated with this listener.
   *
   * @return the binding associated with this listener.
   */
  public EventBinding<? extends ExactPositionListener> getBinding() {
    return binding;
  }

  @Override
  public void onEnter(EventContext context, VirtualFrame frame) {}

  @Override
  public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {}

  @Override
  public void onReturnExceptional(EventContext context, VirtualFrame frame, Throwable exception) {}
}
