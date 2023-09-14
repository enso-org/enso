package org.enso.interpreter.node.expression.builtin.number.decimal;

import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Decimal", name = "abs", description = "Absolute value of a number.")
public class AbsNode extends FloatNode {
  double execute(double self) {
    return Math.abs(self);
  }
}
