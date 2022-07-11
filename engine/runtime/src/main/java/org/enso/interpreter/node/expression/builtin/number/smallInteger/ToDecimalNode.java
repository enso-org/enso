package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "Small_Integer",
    name = "to_decimal",
    description = "Conversion of integers to decimals.")
public class ToDecimalNode extends Node {
  double execute(long self) {
    return self;
  }
}
