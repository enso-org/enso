package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.*;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.builder.FramePointer;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.Function;

@NodeInfo(shortName = "readVar", description = "Access local variable value.")
@NodeField(name = "framePointer", type = FramePointer.class)
public abstract class ReadLocalTargetNode extends ExpressionNode {

  public abstract FramePointer getFramePointer();

  public MaterializedFrame getParentFrame(Frame frame) {
    return Function.ArgumentsHelper.getLocalScope(frame.getArguments());
  }

  @ExplodeLoop
  public MaterializedFrame getProperFrame(Frame frame) {
    MaterializedFrame currentFrame = getParentFrame(frame);
    for (int i = 1; i < getFramePointer().getParentLevel(); i++) {
      currentFrame = getParentFrame(currentFrame);
    }
    return currentFrame;
  }

  @Specialization(rewriteOn = FrameSlotTypeException.class)
  protected long readLong(VirtualFrame frame) throws FrameSlotTypeException {
    if (getFramePointer().getParentLevel() == 0)
      return frame.getLong(getFramePointer().getFrameSlot());
    MaterializedFrame currentFrame = getProperFrame(frame);
    return currentFrame.getLong(getFramePointer().getFrameSlot());
  }

  @Specialization
  protected Object readGeneric(VirtualFrame frame) {
    if (getFramePointer().getParentLevel() == 0)
      return FrameUtil.getObjectSafe(frame, getFramePointer().getFrameSlot());
    MaterializedFrame currentFrame = getProperFrame(frame);
    return FrameUtil.getObjectSafe(currentFrame, getFramePointer().getFrameSlot());
  }
}
