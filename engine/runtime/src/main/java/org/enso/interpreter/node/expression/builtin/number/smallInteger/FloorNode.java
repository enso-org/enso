package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Small_Integer", name = "floor", description = "Small integer floor.")
public class FloorNode extends Node {
  long execute(long _this) {
    return _this;
  }
}
