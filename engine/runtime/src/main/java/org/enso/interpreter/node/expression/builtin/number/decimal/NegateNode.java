package org.enso.interpreter.node.expression.builtin.number.decimal;

import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Float", name = "negate", description = "Negation for numbers.")
public class NegateNode extends FloatNode {
  double execute(double own) {
    return -own;
  }
}
