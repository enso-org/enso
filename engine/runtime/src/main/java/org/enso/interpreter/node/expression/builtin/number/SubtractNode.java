package org.enso.interpreter.node.expression.builtin.number;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Number", name = "-", description = "Subtraction on numbers.")
public class SubtractNode extends Node {
  long execute(long _this, long that) {
    return _this - that;
  }
}
