package org.enso.interpreter.node.local;

import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.StatementNode;

@NodeInfo(shortName = "=", description = "Assigns expression result to a variable.")
@NodeChild(value = "rhsNode", type = ExpressionNode.class)
@NodeField(name = "frameSlot", type = FrameSlot.class)
public abstract class AssignmentNode extends StatementNode {

  public abstract FrameSlot getFrameSlot();

  @Specialization
  protected void writeLong(VirtualFrame frame, long value) {
    frame.getFrameDescriptor().setFrameSlotKind(getFrameSlot(), FrameSlotKind.Long);
    frame.setLong(getFrameSlot(), value);
  }

  @Specialization
  protected void writeObject(VirtualFrame frame, Object value) {
    frame.getFrameDescriptor().setFrameSlotKind(getFrameSlot(), FrameSlotKind.Object);
    frame.setObject(getFrameSlot(), value);
  }
}
