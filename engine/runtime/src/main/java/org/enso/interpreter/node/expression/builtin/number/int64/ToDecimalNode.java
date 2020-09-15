package org.enso.interpreter.node.expression.builtin.number.int64;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "Small_Int",
    name = "to_decimal",
    description = "Conversion of integers to decimals.")
public class ToDecimalNode extends Node {
  double execute(long _this) {
    return _this;
  }
}
