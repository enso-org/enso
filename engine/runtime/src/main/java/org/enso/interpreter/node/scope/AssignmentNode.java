package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.polyglot.RuntimeID;

/** This node represents an assignment to a variable in a given scope. */
@NodeInfo(shortName = "=", description = "Assigns expression result to a variable.")
@NodeChild(value = "rhsNode", type = ExpressionNode.class)
public abstract class AssignmentNode extends ExpressionNode {

  private @CompilerDirectives.CompilationFinal RuntimeID id = null;
  private final int frameSlotIdx;
  private final RuntimeID rhsID;

  AssignmentNode(int frameSlotIdx, RuntimeID rhsID) {
    this.frameSlotIdx = frameSlotIdx;
    this.rhsID = rhsID;
  }

  /**
   * Creates an instance of this node.
   *
   * @param expression the expression being assigned
   * @param frameSlotIdx the slot index to which {@code expression} is being assigned
   * @return a node representing an assignment
   */
  public static AssignmentNode build(ExpressionNode expression, int frameSlotIdx) {
    return AssignmentNodeGen.create(frameSlotIdx, expression.getId(), expression);
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

  @Override
  public RuntimeID getId() {
    return this.id;
  }

  @Override
  public void setId(RuntimeID id) {
    CompilerDirectives.transferToInterpreterAndInvalidate();
    this.id = id;
  }

  public RuntimeID getRhsID() {
    return rhsID;
  }

  boolean isLongOrIllegal(VirtualFrame frame) {
    FrameSlotKind kind = frame.getFrameDescriptor().getSlotKind(frameSlotIdx);
    return kind == FrameSlotKind.Long || kind == FrameSlotKind.Illegal;
  }
}
