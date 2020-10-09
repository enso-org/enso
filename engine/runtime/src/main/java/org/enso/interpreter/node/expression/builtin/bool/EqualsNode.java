package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Boolean", name = "==", description = "Computes the equality of two booleans")
public class EqualsNode extends Node {
  boolean execute(boolean _this, boolean that) {
    return _this == that;
  }
}
