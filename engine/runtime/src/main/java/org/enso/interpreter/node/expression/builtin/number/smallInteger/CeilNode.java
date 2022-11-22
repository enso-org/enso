package org.enso.interpreter.node.expression.builtin.number.smallInteger;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Small_Integer", name = "ceil", description = "Small integer ceiling.")
public class CeilNode extends Node {
  long execute(long self) {
    return self;
  }
}
