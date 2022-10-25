package org.enso.interpreter.test;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.tag.IdentifiedTag;

import java.util.UUID;

/**
 * A debug instrument used to test code locations.
 *
 * <p>Allows to listen for a node with a given id, and later verify if such a node was indeed
 * encountered in the course of execution.
 */
@TruffleInstrument.Registration(
    id = CodeIdsTestInstrument.INSTRUMENT_ID,
    services = CodeIdsTestInstrument.class)
public class CodeIdsTestInstrument extends TruffleInstrument {
  public static final String INSTRUMENT_ID = "ids-test";
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
   * the one expected by the user.
   */
  public static class IdEventListener implements ExecutionEventListener {
    private boolean successful = false;
    private final UUID expectedId;
    private final String expectedResult;

    public IdEventListener(UUID expectedId, String expectedResult) {
      this.expectedId = expectedId;
      this.expectedResult = expectedResult;
    }

    public UUID getId() {
      return expectedId;
    }

    /**
     * Was a node with parameters specified for this listener encountered in the course of
     * execution?
     *
     * @return {@code true} if the requested node was observed, {@code false} otherwise
     */
    public boolean isSuccessful() {
      return successful;
    }

    public String getExpectedResult() {
      return expectedResult;
    }

    @Override
    public void onEnter(EventContext context, VirtualFrame frame) {}

    /**
     * Checks if the node to be executed is the node this listener was created to observe.
     *
     * @param context current execution context
     * @param frame current execution frame
     * @param result the result of executing the node
     */
    @Override
    public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {
      if (successful) {
        return;
      }
      Node node = context.getInstrumentedNode();
      if (!(node instanceof ExpressionNode)) {
        return;
      }
      UUID id = ((ExpressionNode) node).getId();
      if (!id.equals(expectedId)) {
        return;
      }
      if (expectedResult != null && expectedResult.equals(result.toString())) {
        successful = true;
      }
    }

    /**
     * Checks if the specified was called, if its execution triggered TCO.
     *
     * @param context current execution context.
     * @param frame current execution frame.
     * @param exception the exception thrown from this node's execution.
     */
    @Override
    public void onReturnExceptional(EventContext context, VirtualFrame frame, Throwable exception) {
      if (!(exception instanceof TailCallException)) {
        return;
      }
      if (!(context.getInstrumentedNode() instanceof ExpressionNode)) {
        return;
      }
      UUID id = ((ExpressionNode) context.getInstrumentedNode()).getId();
      if (expectedResult == null) {
        successful = true;
      }
    }
  }

  /**
   * Attaches a new listener to observe nodes with given parameters.
   *
   * @param id the ID of nodes to observe
   * @param expectedResult the string representation of the result expected by the execution of
   *     observed node.
   * @return a reference to attached event listener
   */
  public EventBinding<IdEventListener> bindTo(UUID id, String expectedResult) {
    return env.getInstrumenter()
        .attachExecutionEventListener(
            SourceSectionFilter.newBuilder().tagIs(IdentifiedTag.class).build(),
            new IdEventListener(id, expectedResult));
  }

  /**
   * Attaches a new listener to observe nodes that are supposed to trigger TCO.
   *
   * @param id the ID of nodes to observe.
   * @return a reference to the attached event listener.
   */
  public EventBinding<IdEventListener> bindToTailCall(UUID id) {
    return bindTo(id, null);
  }
}
