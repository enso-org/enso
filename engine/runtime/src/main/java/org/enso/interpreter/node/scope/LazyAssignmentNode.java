package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.function.Function;

/** This node represents an assignment to a variable in a given scope. */
@NodeInfo(shortName = "=", description = "Lazily assigns expression result to a variable.")
@NodeChild(value = "rhsNode", type = ExpressionNode.class)
public abstract class LazyAssignmentNode extends ExpressionNode {

  private final int parentLevel;
  private final int frameSlotIdx;

  LazyAssignmentNode(int parentLevel, int frameSlotIdx) {
    this.parentLevel = parentLevel;
    this.frameSlotIdx = frameSlotIdx;
  }

  /**
   * Creates an instance of this node.
   *
   * @param expression the expression being assigned
   * @param frameSlotIdx the slot index to which {@code expression} is being assigned
   * @return a node representing an assignment
   */
  public static LazyAssignmentNode build(
      ExpressionNode expression, int parentLevel, int frameSlotIdx) {
    return LazyAssignmentNodeGen.create(parentLevel, frameSlotIdx, expression);
  }

  /**
   * Writes a long value into the provided frame.
   *
   * @param frame the frame to write to
   * @param value the value to write
   * @return the unit type
   */
  @Specialization(guards = "isLongOrIllegal(frame)")
  protected Object writeLong(VirtualFrame frame, long value) {
    var realFrame = getParentFrame(frame);
    realFrame.getFrameDescriptor().setSlotKind(frameSlotIdx, FrameSlotKind.Long);
    realFrame.setLong(frameSlotIdx, value);
    return value;
  }

  /**
   * Writes an object value into the provided frame.
   *
   * @param frame the frame to write to
   * @param value the value to write
   * @return the unit type
   */
  @Fallback
  protected Object writeObject(VirtualFrame frame, Object value) {
    var realFrame = getParentFrame(frame);
    realFrame.getFrameDescriptor().setSlotKind(frameSlotIdx, FrameSlotKind.Object);
    realFrame.setObject(frameSlotIdx, value);

    return value;
  }

  boolean isLongOrIllegal(VirtualFrame frame) {
    var realFrame = getParentFrame(frame);
    FrameSlotKind kind = realFrame.getFrameDescriptor().getSlotKind(frameSlotIdx);
    return kind == FrameSlotKind.Long || kind == FrameSlotKind.Illegal;
  }

  private MaterializedFrame getParentFrame(Frame frame) {
    return Function.ArgumentsHelper.getLocalScope(frame.getArguments());
  }

  private MaterializedFrame getProperFrame(Frame frame) {
    MaterializedFrame currentFrame = getParentFrame(frame);
    for (int i = 1; i < parentLevel; i++) {
      currentFrame = getParentFrame(currentFrame);
    }
    return currentFrame;
  }
}
