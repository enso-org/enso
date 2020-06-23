package org.enso.interpreter.node.expression.builtin.number;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Number", name = "*", description = "Multiplication on numbers.")
public class MultiplyNode extends Node {
  public long execute(long self, long that) {
    return self * that;
  }
}
