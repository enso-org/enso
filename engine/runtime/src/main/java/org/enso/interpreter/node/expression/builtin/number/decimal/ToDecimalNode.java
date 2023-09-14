package org.enso.interpreter.node.expression.builtin.number.decimal;

import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Decimal", name = "to_decimal", description = "Identity on decimals")
public class ToDecimalNode extends FloatNode {
  double execute(double self) {
    return self;
  }
}
