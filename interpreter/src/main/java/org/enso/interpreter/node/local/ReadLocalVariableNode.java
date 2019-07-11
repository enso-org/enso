package org.enso.interpreter.node.local;

import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.*;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.Function;

@NodeInfo(shortName = "readVar", description = "Access local variable value.")
@NodeField(name = "slot", type = FrameSlot.class)
@NodeField(name = "parentLevel", type = int.class)
public abstract class ReadLocalVariableNode extends ExpressionNode {
  public abstract int getParentLevel();

  public abstract FrameSlot getSlot();

  public MaterializedFrame getParentFrame(Frame frame) {
    return Function.ArgumentsHelper.getLocalScope(frame.getArguments());
  }

  @ExplodeLoop
  public MaterializedFrame getProperFrame(Frame frame) {
    MaterializedFrame currentFrame = getParentFrame(frame);
    for (int i = 1; i < getParentLevel(); i++) {
      currentFrame = getParentFrame(currentFrame);
    }
    return currentFrame;
  }

  @Specialization(rewriteOn = FrameSlotTypeException.class)
  protected long readLong(VirtualFrame frame) throws FrameSlotTypeException {
    if (getParentLevel() == 0) return frame.getLong(getSlot());
    MaterializedFrame currentFrame = getProperFrame(frame);
    return currentFrame.getLong(getSlot());
  }

  @Specialization
  protected Object readGeneric(VirtualFrame frame) {
    if (getParentLevel() == 0) return FrameUtil.getObjectSafe(frame, getSlot());
    MaterializedFrame currentFrame = getProperFrame(frame);
    return FrameUtil.getObjectSafe(currentFrame, getSlot());
  }
}
