package org.enso.interpreter.node.expression.builtin.number.decimal;

import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Float", name = "abs", description = "Absolute value of a number.")
public final class AbsNode extends FloatNode {
  private AbsNode() {}

  static AbsNode build() {
    return new AbsNode();
  }

  double execute(double own) {
    return Math.abs(own);
  }
}
