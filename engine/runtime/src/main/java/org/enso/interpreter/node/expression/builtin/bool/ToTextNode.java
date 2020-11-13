package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Boolean", name = "to_text", description = "Boolean to text conversion.")
public class ToTextNode extends Node {
  String execute(boolean _this) {
    return _this ? "True" : "False";
  }
}
