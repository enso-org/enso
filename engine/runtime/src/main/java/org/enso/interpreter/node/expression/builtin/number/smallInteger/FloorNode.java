package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Small_Integer", name = "floor", description = "Small integer floor.")
public class FloorNode extends Node {
  Object execute(long _this) {
    return _this;
  }
}
