package org.enso.interpreter.node.expression.builtin.number;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Number", name = "%", description = "Modulo operation for numbers.")
public class ModNode extends Node {
  long execute(long _this, long that) {
    return _this % that;
  }
}
