package org.enso.interpreter.node.expression.builtin.number;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Number", name = "-", description = "Subtraction on numbers.")
public class SubtractNode extends Node {
  public long execute(long self, long that) {
    return self - that;
  }
}
