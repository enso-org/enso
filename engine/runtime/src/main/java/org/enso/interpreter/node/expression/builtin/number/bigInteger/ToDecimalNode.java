package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
    type = "Big_Integer",
    name = "to_decimal",
    description = "Conversion of integers to decimals")
public class ToDecimalNode extends Node {
  double execute(EnsoBigInteger _this) {
    return BigIntegerOps.toDouble(_this.getValue());
  }
}
