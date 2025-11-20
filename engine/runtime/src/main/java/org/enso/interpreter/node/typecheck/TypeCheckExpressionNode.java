package org.enso.interpreter.node.typecheck;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.polyglot.RuntimeID;

final class TypeCheckExpressionNode extends ExpressionNode {

  @Child private ExpressionNode original;
  @Child private TypeCheckValueNode check;
  private @CompilerDirectives.CompilationFinal RuntimeID id = null;

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
    var result = check.handleCheckOrConversion(frame, value);
    return result;
  }

  @Override
  public boolean isInstrumentable() {
    return false;
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
}
