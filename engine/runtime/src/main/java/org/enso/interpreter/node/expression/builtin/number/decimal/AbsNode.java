package org.enso.interpreter.node.expression.builtin.number.decimal;

import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Float", name = "abs", description = "Absolute value of a number.")
public class AbsNode extends FloatNode {
  double execute(double own) {
    return Math.abs(own);
  }
}
