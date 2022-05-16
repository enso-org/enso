package org.enso.interpreter.node.expression.builtin.special;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Ref;

@BuiltinMethod(type = "Special", name = "<new_ref>")
public class NewRefNode extends Node {
  public Ref execute() {
    return new Ref(null);
  }
}
