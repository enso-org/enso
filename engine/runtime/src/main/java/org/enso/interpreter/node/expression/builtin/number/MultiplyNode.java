package org.enso.interpreter.node.expression.builtin.number;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Number", name = "*", description = "Multiplication on numbers.")
public class MultiplyNode extends Node {
  long execute(long _this, long that) {
    return _this * that;
  }
}
