package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.NodeUtil;
import java.util.Set;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.EnsoContext;

/** This node represents an assignment to a variable in a given scope. */
@NodeInfo(shortName = "=", description = "Assigns expression result to a variable.")
@NodeField(name = "name", type = String.class)
@NodeField(name = "assignmentBounds", type = int[].class)
@NodeChild(value = "rhsNode", type = ExpressionNode.class)
public abstract class AssignmentNode extends ExpressionNode {

  private final int frameSlotIdx;

  abstract String getName();

  abstract int[] getAssignmentBounds();

  abstract ExpressionNode getRhsNode();

  AssignmentNode(int frameSlotIdx) {
    this.frameSlotIdx = frameSlotIdx;
  }

  /**
   * Creates an instance of this node.
   *
   * @param name the name of the variable to assign to
   * @param bounds the location of the assignment for {@link StandardTags.WriteVariableTag}
   *     instrumentation
   * @param expression the expression being assigned
   * @param frameSlotIdx the slot index to which {@code expression} is being assigned
   * @return a node representing an assignment
   */
  public static AssignmentNode build(
      String name, int[] bounds, ExpressionNode expression, int frameSlotIdx) {
    return AssignmentNodeGen.create(frameSlotIdx, expression, name, bounds);
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
    frame.getFrameDescriptor().setSlotKind(frameSlotIdx, FrameSlotKind.Long);
    frame.setLong(frameSlotIdx, value);

    return EnsoContext.get(this).getNothing();
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
    frame.getFrameDescriptor().setSlotKind(frameSlotIdx, FrameSlotKind.Object);
    frame.setObject(frameSlotIdx, value);

    return EnsoContext.get(this).getNothing();
  }

  boolean isLongOrIllegal(VirtualFrame frame) {
    FrameSlotKind kind = frame.getFrameDescriptor().getSlotKind(frameSlotIdx);
    return kind == FrameSlotKind.Long || kind == FrameSlotKind.Illegal;
  }

  @Override
  public InstrumentableNode materializeInstrumentableNodes(
      Set<Class<? extends Tag>> materializedTags) {
    if (materializedTags.contains(StandardTags.WriteVariableTag.class)) {
      var rhs = getRhsNode();
      var bounds = getAssignmentBounds();
      if (bounds == null) {
        bounds = getSourceSectionBounds();
      }
      if (bounds != null && !isNodeWrapped(rhs)) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        var newRhs = new AssignLocalVariableNode(getName(), rhs);
        newRhs.setSourceLocation(bounds[0], bounds[1]);
        var res = NodeUtil.replaceChild(this, getRhsNode(), newRhs);
        insert(newRhs);
        notifyInserted(newRhs);
        assert res;
      }
    }
    return this;
  }

  private static boolean isNodeWrapped(ExpressionNode node) {
    return node instanceof AssignLocalVariableNode || ExpressionNode.isWrapper(node);
  }
}
