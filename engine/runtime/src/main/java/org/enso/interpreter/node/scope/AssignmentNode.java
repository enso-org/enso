package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.polyglot.RuntimeID;

/** This node represents an assignment to a variable in a given scope. */
@NodeInfo(shortName = "=", description = "Assigns expression result to a variable.")
public final class AssignmentNode extends ExpressionNode {

  @Child ExpressionNode rhsNode;

  private @CompilerDirectives.CompilationFinal RuntimeID id = null;
  private final int frameSlotIdx;
  private final RuntimeID rhsID;

  AssignmentNode(int frameSlotIdx, ExpressionNode rhsNode) {
    this.frameSlotIdx = frameSlotIdx;
    this.rhsID = rhsNode.getId();
    this.rhsNode = rhsNode;
  }

  /**
   * Creates an instance of this node.
   *
   * @param expression the expression being assigned
   * @param frameSlotIdx the slot index to which {@code expression} is being assigned
   * @return a node representing an assignment
   */
  public static AssignmentNode build(ExpressionNode expression, int frameSlotIdx) {
    return new AssignmentNode(frameSlotIdx, expression);
  }

  public Object executeGeneric(VirtualFrame frame) {
    var result = rhsNode.executeGeneric(frame);
    frame.getFrameDescriptor().setSlotKind(frameSlotIdx, FrameSlotKind.Object);
    frame.setObject(frameSlotIdx, result);
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
