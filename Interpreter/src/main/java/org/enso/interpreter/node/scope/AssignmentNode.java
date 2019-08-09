package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

/** This node represents an assignment to a variable in a given scope. */
@NodeInfo(shortName = "=", description = "Assigns expression result to a variable.")
@NodeChild(value = "rhsNode", type = ExpressionNode.class)
@NodeField(name = "frameSlot", type = FrameSlot.class)
public abstract class AssignmentNode extends ExpressionNode {
  /**
   * Writes a long value into the provided frame.
   *
   * @param frame the frame to write to
   * @param value the value to write
   * @return the {@link AtomConstructor#UNIT unit} type
   */
  @Specialization
  protected Object writeLong(VirtualFrame frame, long value) {
    frame.getFrameDescriptor().setFrameSlotKind(getFrameSlot(), FrameSlotKind.Long);
    frame.setLong(getFrameSlot(), value);

    return AtomConstructor.UNIT.newInstance();
  }

  /**
   * Writes an object value into the provided frame.
   *
   * @param frame the frame to write to
   * @param value the value to write
   * @return the {@link AtomConstructor#UNIT unit} type
   */
  @Specialization
  protected Object writeObject(VirtualFrame frame, Object value) {
    frame.getFrameDescriptor().setFrameSlotKind(getFrameSlot(), FrameSlotKind.Object);
    frame.setObject(getFrameSlot(), value);

    return AtomConstructor.UNIT.newInstance();
  }

  /**
   * Gets the current frame slot
   *
   * @return the frame slot being written to
   */
  public abstract FrameSlot getFrameSlot();
}
