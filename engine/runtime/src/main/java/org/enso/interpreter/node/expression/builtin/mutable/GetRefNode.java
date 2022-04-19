package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Ref;

@BuiltinMethod(type = "Ref", name = "get", description = "Gets the value stored in the reference.")
public class GetRefNode extends Node {

  Object execute(Object _this, Ref ref) {
    return ref.getValue();
  }
}
