package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Decimal", name = "abs", description = "Absolute value of a number.")
public class AbsNode extends Node {
  double execute(double self) {
    return Math.abs(self);
  }
}
