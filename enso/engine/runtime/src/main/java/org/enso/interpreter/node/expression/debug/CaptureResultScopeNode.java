package org.enso.interpreter.node.expression.debug;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.CaptureCallerInfoNode;
import org.enso.interpreter.runtime.callable.CallerInfo;

/** Node capturing the runtime execution scope of its child. */
@NodeInfo(shortName = "ScopeCapture", description = "Captures the child's execution scope.")
public class CaptureResultScopeNode extends ExpressionNode {

  /** Value object wrapping the expression return value and the execution scope. */
  public static class WithCallerInfo {
    private final CallerInfo callerInfo;
    private final Object result;

    private WithCallerInfo(CallerInfo callerInfo, Object result) {
      this.callerInfo = callerInfo;
      this.result = result;
    }

    /**
     * Gets the attached caller info object.
     *
     * @return the caller info (execution scope) captured by this node
     */
    public CallerInfo getCallerInfo() {
      return callerInfo;
    }

    /**
     * Gets the attached result object.
     *
     * @return the result captured by this node
     */
    public Object getResult() {
      return result;
    }
  }

  private @Child ExpressionNode expression;
  private @Child CaptureCallerInfoNode captureCallerInfoNode = CaptureCallerInfoNode.build();

  private CaptureResultScopeNode(ExpressionNode expression) {
    this.expression = expression;
  }

  /**
   * Create a new instance of this node.
   *
   * @param expressionNode the child of this node, the return value of which should be captured
   * @return an instance of this node
   */
  public static CaptureResultScopeNode build(ExpressionNode expressionNode) {
    return new CaptureResultScopeNode(expressionNode);
  }

  /**
   * Executes the node by running the child expression and capturing the caller info object
   * containing current scope information.
   *
   * @param frame the stack frame for execution
   * @return the captured caller info and the return value of the child expression
   */
  @Override
  public WithCallerInfo executeGeneric(VirtualFrame frame) {
    return new WithCallerInfo(
        captureCallerInfoNode.execute(frame.materialize()), expression.executeGeneric(frame));
  }
}
