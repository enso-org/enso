package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Ref;

@BuiltinMethod(type = "Ref", name = "new", description = "Creates a new ref.")
public class NewRefNode extends Node {

  Object execute(Object _this, Object value) {
    return new Ref(value);
  }
}
