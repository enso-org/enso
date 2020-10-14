package org.enso.interpreter.node.callable.thunk;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.state.Stateful;

/** Node responsible for handling user-requested thunks forcing. */
@NodeInfo(shortName = "Force", description = "Forces execution of a thunk at runtime")
@NodeChild(value = "target", type = ExpressionNode.class)
public abstract class ForceNode extends ExpressionNode {

  ForceNode() {}

  /**
   * Creates an instance of this node.
   *
   * @param target the expression being forced
   * @return a node representing {@code target} being forced
   */
  public static ForceNode build(ExpressionNode target) {
    return ForceNodeGen.create(target);
  }

  @Specialization
  Object passToExecutorNode(
      VirtualFrame frame,
      Thunk thunk,
      @Cached("build()") ThunkExecutorNode thunkExecutorNode) {
    Object state = FrameUtil.getObjectSafe(frame, getStateFrameSlot());
    Stateful result = thunkExecutorNode.executeThunk(thunk, state, getTailStatus());
    frame.setObject(getStateFrameSlot(), result.getState());
    return result.getValue();
  }
}
