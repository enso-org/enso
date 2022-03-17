package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.Context;

/** This node represents an assignment to a variable in a given scope. */
@NodeInfo(shortName = "=", description = "Assigns expression result to a variable.")
@NodeChild(value = "rhsNode", type = ExpressionNode.class)
@NodeField(name = "frameSlot", type = FrameSlot.class)
public abstract class AssignmentNode extends ExpressionNode {

  AssignmentNode() {}

  /**
   * Creates an instance of this node.
   *
   * @param expression the expression being assigned
   * @param slot the slot to which {@code expression} is being assigned
   * @return a node representing an assignment
   */
  public static AssignmentNode build(ExpressionNode expression, FrameSlot slot) {
    return AssignmentNodeGen.create(expression, slot);
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
    frame.getFrameDescriptor().setFrameSlotKind(getFrameSlot(), FrameSlotKind.Long);
    frame.setLong(getFrameSlot(), value);

    return Context.get(this).getNothing().newInstance();
  }

  /**
   * Writes an object value into the provided frame.
   *
   * @param frame the frame to write to
   * @param value the value to write
   * @return the unit type
   */
  @Specialization
  protected Object writeObject(VirtualFrame frame, Object value) {
    frame.getFrameDescriptor().setFrameSlotKind(getFrameSlot(), FrameSlotKind.Object);
    frame.setObject(getFrameSlot(), value);

    return Context.get(this).getNothing().newInstance();
  }

  boolean isLongOrIllegal(VirtualFrame frame) {
    FrameSlotKind kind = frame.getFrameDescriptor().getFrameSlotKind(getFrameSlot());
    return kind == FrameSlotKind.Long || kind == FrameSlotKind.Illegal;
  }

  /**
   * Gets the current frame slot
   *
   * @return the frame slot being written to
   */
  public abstract FrameSlot getFrameSlot();
}
