package org.enso.interpreter.node.callable.thunk;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.argument.Thunk;

/** This node is responsible for wrapping a call target in a {@link Thunk} at execution time. */
@NodeInfo(shortName = "CreateThunk", description = "Wraps a call target in a thunk at runtime")
public class CreateThunkNode extends ExpressionNode {
  private final RootCallTarget callTarget;

  private CreateThunkNode(RootCallTarget callTarget) {
    this.callTarget = callTarget;
  }

  /**
   * Executes the node, creating a {@link Thunk} that wraps the internal {@link
   * com.oracle.truffle.api.CallTarget}.
   *
   * @param frame the stack frame for execution
   * @return the thunk
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return new Thunk(this.callTarget, frame.materialize());
  }

  /**
   * Creates a new {@link CreateThunkNode}.
   *
   * @param callTarget the call target to wrap into a {@link Thunk}.
   * @return the node
   */
  public static CreateThunkNode build(RootCallTarget callTarget) {
    return new CreateThunkNode(callTarget);
  }
}
