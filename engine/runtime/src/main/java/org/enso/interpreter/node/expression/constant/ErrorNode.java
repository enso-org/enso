package org.enso.interpreter.node.expression.constant;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.TruffleObject;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.error.PanicException;

/** Throws a runtime panic containing a statically-known payload. */
public class ErrorNode extends ExpressionNode {
  private final TruffleObject payload;

  private ErrorNode(TruffleObject payload) {
    this.payload = payload;
  }

  /**
   * Executes the node, by throwing a new panic containing the specified payload.
   *
   * @param frame the stack frame for execution
   * @return never returns
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    throw new PanicException(payload, this);
  }

  /**
   * Create a new instance of this node.
   *
   * @param payload the payload carried by exceptions thrown in the course of this node's execution.
   * @return a new instance of this node.
   */
  public static ErrorNode build(TruffleObject payload) {
    return new ErrorNode(payload);
  }
}
