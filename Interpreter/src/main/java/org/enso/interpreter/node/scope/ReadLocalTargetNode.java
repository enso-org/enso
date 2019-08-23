package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.*;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.scope.FramePointer;

/** Reads from a local target (variable or call target). */
@NodeInfo(shortName = "readVar", description = "Access local variable value.")
@NodeField(name = "framePointer", type = FramePointer.class)
public abstract class ReadLocalTargetNode extends ExpressionNode {
  public abstract FramePointer getFramePointer();

  /**
   * Reads a {@code long} value from the frame.
   *
   * @param frame the stack frame to read from
   * @return the value read from the appropriate slot in {@code frame}
   * @throws FrameSlotTypeException when the specified frame slot does not contain a value with the
   *     expected type
   */
  @Specialization(rewriteOn = FrameSlotTypeException.class)
  protected long readLong(VirtualFrame frame) throws FrameSlotTypeException {
    if (getFramePointer().getParentLevel() == 0)
      return frame.getLong(getFramePointer().getFrameSlot());
    MaterializedFrame currentFrame = getProperFrame(frame);
    return currentFrame.getLong(getFramePointer().getFrameSlot());
  }

  /**
   * Reads an generic value from the frame.
   *
   * @param frame the stack frame to read from
   * @return the value read from the appropriate slot in {@code frame}
   * @throws FrameSlotTypeException when the specified frame slot does not contain a value with the
   *     expected type
   */
  @Specialization
  protected Object readGeneric(VirtualFrame frame) {
    if (getFramePointer().getParentLevel() == 0)
      return FrameUtil.getObjectSafe(frame, getFramePointer().getFrameSlot());
    MaterializedFrame currentFrame = getProperFrame(frame);
    return FrameUtil.getObjectSafe(currentFrame, getFramePointer().getFrameSlot());
  }

  /**
   * Obtains the direct parent frame for a given frame.
   *
   * @param frame the frame whose parent needs to be found
   * @return the parent frame of {@code frame}
   */
  public MaterializedFrame getParentFrame(Frame frame) {
    return Function.ArgumentsHelper.getLocalScope(frame.getArguments());
  }

  /**
   * Gets the Enso parent frame for a given frame.
   *
   * <p>This method is responsible for getting the guest language parent frame for the current frame
   * by walking up the stack based on the scope in which the function was defined.
   *
   * @param frame the frame to find the Enso parent frame for
   * @return the guest language parent frame of {@code frame}
   */
  @ExplodeLoop
  public MaterializedFrame getProperFrame(Frame frame) {
    MaterializedFrame currentFrame = getParentFrame(frame);
    for (int i = 1; i < getFramePointer().getParentLevel(); i++) {
      currentFrame = getParentFrame(currentFrame);
    }
    return currentFrame;
  }
}
