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
    // return AssignmentNodeGen.create(frameSlotIdx, expression);
    return new AssignmentNode(frameSlotIdx, expression);
  }

  public Object executeGeneric(VirtualFrame frame) {
    var analysis = EnsoContext.get(this).currentRuntimeAnalysis();
    var ref = analysis.startExecutingCachedExpression(getId());
    try {
      var result = rhsNode.executeGeneric(frame);
      ref.update(result);
      frame.getFrameDescriptor().setSlotKind(frameSlotIdx, FrameSlotKind.Object);
      frame.setObject(frameSlotIdx, ref);
    } finally {
      analysis.endExecutingCachedExpression(getId());
    }
    return EnsoContext.get(this).getNothing();
  }

  /**
   * Writes a long value into the provided frame.
   *
   * @param frame the frame to write to
   * @param value the value to write
   * @return the unit type
   */
  /*@Specialization(guards = "isLongOrIllegal(frame)")
  protected Object writeLong(VirtualFrame frame, long value) {

    var ref =  EnsoContext.get(this).currentRuntimeAnalysis().currentlyExecutingExpression();
    System.out.println("Updating " + getId() + " long with " + value + " and ref " + ref);
    if (ref != null) {
        ref.update(value);
        frame.getFrameDescriptor().setSlotKind(frameSlotIdx, FrameSlotKind.Object);
        frame.setObject(frameSlotIdx, ref);
    } else {
        frame.getFrameDescriptor().setSlotKind(frameSlotIdx, FrameSlotKind.Long);
        frame.setObject(frameSlotIdx, value);
    }

    return EnsoContext.get(this).getNothing();
  }*/

  /**
   * Writes an object value into the provided frame.
   *
   * @param frame the frame to write to
   * @param value the value to write
   * @return the unit type
   */
  /*@Fallback
  protected Object writeObject(VirtualFrame frame, Object value) {

    var ref =  EnsoContext.get(this).currentRuntimeAnalysis().currentlyExecutingExpression();
    //var ref = new RefObject(getId(), value);
    System.out.println("Updating " + getId() + " with " + value);
    frame.getFrameDescriptor().setSlotKind(frameSlotIdx, FrameSlotKind.Object);
    if (ref != null) {
        ref.update(value);
        frame.setObject(frameSlotIdx, ref);
    } else {
        frame.setObject(frameSlotIdx, value);
    }

    return EnsoContext.get(this).getNothing();
  }*/

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
