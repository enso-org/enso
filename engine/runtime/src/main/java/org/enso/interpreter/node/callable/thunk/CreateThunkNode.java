package org.enso.interpreter.node.callable.thunk;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.argument.Thunk;

public class CreateThunkNode extends ExpressionNode {
  private final RootCallTarget callTarget;

  private CreateThunkNode(RootCallTarget callTarget) {
    this.callTarget = callTarget;
  }

  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return new Thunk(this.callTarget, frame.materialize());
  }

  public static CreateThunkNode build(RootCallTarget callTarget) {
    return new CreateThunkNode(callTarget);
  }
}
