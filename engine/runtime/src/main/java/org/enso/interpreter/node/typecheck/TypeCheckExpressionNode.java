package org.enso.interpreter.node.typecheck;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.node.ExpressionNode;

final class TypeCheckExpressionNode extends ExpressionNode {

  @Child private ExpressionNode original;
  @Child private TypeCheckValueNode check;

  TypeCheckExpressionNode(ExpressionNode original, TypeCheckValueNode check) {
    this.check = check;
    this.original = original;
  }

  ExpressionNode getOriginal() {
    return original;
  }

  @Override
  public Object executeGeneric(VirtualFrame frame) {
    var value = original.executeGeneric(frame);
    var result = check.handleCheckOrConversion(frame, value, original);
    return result;
  }

  @Override
  public boolean isInstrumentable() {
    return false;
  }
}
