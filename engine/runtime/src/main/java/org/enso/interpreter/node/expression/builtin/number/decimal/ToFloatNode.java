package org.enso.interpreter.node.expression.builtin.number.decimal;

import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Float", name = "to_decimal", description = "Identity on decimals")
public class ToFloatNode extends FloatNode {
  double execute(double self) {
    return self;
  }
}
