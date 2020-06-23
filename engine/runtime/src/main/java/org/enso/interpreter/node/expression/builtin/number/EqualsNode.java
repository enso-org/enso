package org.enso.interpreter.node.expression.builtin.number;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Number", name = "==", description = "Equality on numbers.")
public class EqualsNode extends Node {
  public boolean execute(long self, long that) {
    return self == that;
  }
}
