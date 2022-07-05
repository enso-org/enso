package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Decimal", name = "negate", description = "Negation for numbers.")
public class NegateNode extends Node {
  double execute(double self) {
    return -self;
  }
}
