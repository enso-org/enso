package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "floor", description = "Big integer floor.")
public class FloorNode extends Node {
  Object execute(EnsoBigInteger self) {
    return self;
  }
}
