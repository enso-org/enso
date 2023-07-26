package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Float", name = "to_float", description = "Identity on floats")
public class ToFloatNode extends Node {
  double execute(double self) {
    return self;
  }
}
