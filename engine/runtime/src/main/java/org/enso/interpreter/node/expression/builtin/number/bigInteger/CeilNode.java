package org.enso.interpreter.node.expression.builtin.number.bigInteger;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Big_Integer", name = "ceil", description = "Big integer ceiling.")
public class CeilNode extends Node {
  Object execute(EnsoBigInteger _this) {
    return _this;
  }
}
