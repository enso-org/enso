package org.enso.interpreter.test;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.api.nodes.Node;
import java.util.LinkedHashMap;
import java.util.Map;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.control.TailCallException;

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
  public static class IdEventListener implements ExecutionEventNodeFactory {
    private boolean successful = false;
    private final UUID expectedId;
    private final String expectedResult;
    private final Map<IdEventNode, Object> nodes = new LinkedHashMap<>();

    IdEventListener(UUID expectedId, String expectedResult) {
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

    public String dumpNodes() {
      var sb = new StringBuilder();
      for (var n : nodes.entrySet()) {
        sb.append("\nvalue ").append(n.getValue()).append("  for " ).append(n.getKey().toString());
      }
      return sb.toString();
    }

    @Override
    public ExecutionEventNode create(EventContext context) {
      var node = new IdEventNode(context);
      nodes.put(node, null);
      return node;
    }

    private final class IdEventNode extends ExecutionEventNode {
      private final EventContext context;

      IdEventNode(EventContext context) {
        this.context = context;
      }

      @Override
      public void onEnter(VirtualFrame frame) {}

      /**
       * Checks if the node to be executed is the node this listener was created to observe.
       *
       * @param context current execution context
       * @param frame current execution frame
       * @param result the result of executing the node
       */
      @Override
      public void onReturnValue(VirtualFrame frame, Object result) {
        if (successful) {
          return;
        }
        Node node = context.getInstrumentedNode();
        if (!(node instanceof ExpressionNode)) {
          return;
        }
        nodes.put(this, result);
        UUID id = ((ExpressionNode) node).getId();
        if (id == null || !id.equals(expectedId)) {
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
      public void onReturnExceptional(VirtualFrame frame, Throwable exception) {
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

      @Override
      public String toString() {
        var sb = new StringBuilder();
        sb.append(context.getInstrumentedNode().getClass().getSimpleName());
        if (context.getInstrumentedNode() instanceof ExpressionNode expr) {
            sb.append("@").append(expr.getId());
        }
        sb.append(" ");
        sb.append(context.getInstrumentedSourceSection());
        return sb.toString();
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
    var testSource = SourceFilter.newBuilder().sourceIs((t) -> t.getName().equals("Test")).build();
    var eventFilter =
        SourceSectionFilter.newBuilder()
            .sourceFilter(testSource)
            .build();
    var factory = new IdEventListener(id, expectedResult);
    return env.getInstrumenter().attachExecutionEventFactory(eventFilter, factory);
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
