package org.enso.interpreter.node.typecheck;

import com.oracle.truffle.api.frame.VirtualFrame;

final class FailCheckNode extends AbstractTypeCheckNode {
  FailCheckNode(String comment) {
    super(comment);
  }

  @Override
  Object findDirectMatch(VirtualFrame frame, Object value) {
    return null;
  }

  @Override
  Object executeConversion(VirtualFrame frame, Object value) {
    return null;
  }

  @Override
  String expectedTypeMessage() {
    return "resolved to a type";
  }
}
