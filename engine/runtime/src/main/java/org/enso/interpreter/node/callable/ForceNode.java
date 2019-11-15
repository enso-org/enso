package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.argument.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.state.Stateful;

/** Node responsible for handling user-requested thunks forcing. */
@NodeChild(value = "target", type = ExpressionNode.class)
public abstract class ForceNode extends ExpressionNode {
  @Specialization
  Object passToExecutorNode(
      VirtualFrame frame,
      Thunk thunk,
      @Cached("build(isTail())") ThunkExecutorNode thunkExecutorNode) {
    Object state = FrameUtil.getObjectSafe(frame, getStateFrameSlot());
    Stateful result = thunkExecutorNode.executeThunk(thunk, state);
    frame.setObject(getStateFrameSlot(), result.getState());
    return result.getValue();
  }
}
