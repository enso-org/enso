package org.enso.interpreter.node.expression.builtin.number;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Number", name = "negate", description = "Negation for numbers.")
public class NegateNode extends Node {
  long execute(long _this) {
    return -_this;
  }
}
