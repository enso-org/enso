package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Text", name = "+", description = "Text concatenation.")
public class ConcatNode extends Node {
  String execute(String _this, String that) {
    return _this + that;
  }
}
